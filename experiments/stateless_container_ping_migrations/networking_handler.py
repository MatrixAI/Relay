import os
import multiaddress as ma
import base64
import random
import time
import sys
'''
Module for interacting with the linux networking stack and iptables itself.

ramwan  <ray.wan@matrix.ai>
'''

'''
- OVERALL STRUCTURE -

Each 'service' will be running in a separate network namespace connected by
a veth pair to a linux bridge running in its own network namespace.

Note that when cleaning up the network spaces, a veth pair will be removed if a
netns which one end resides in is removed as well.
'''

'''
- DATA STRUCTURES -

net_map
(key)           (value)
multiaddress    (netns_name, multiaddress)
'''

_net_map = {}

bridge_netns = "orchestrator"
bridge_name = "the_bridge"
bridge_ipv4_multiaddr = ma.pack("/ipv4/10.0.0.1")
#bridge_ipv6_multiaddr = "/ipv6/ff:ff::"

subnet_mask = "/24"

def _name():
    '''
    Return a random string.
    '''
    seed = random.randint(-10000, 10000)
    return base64.b64encode( str(seed).encode('utf-8') ).decode('utf-8')

def _conn_ns_bridge(netns, veth_end):
    '''
    Set a veth end to a network namespace.
    '''
    ret = os.system('ip netns exec '+bridge_netns+ \
                   ' ip link set '+veth_end+' netns '+ netns)

def _slave_veth(veth_end):
    '''
    Set a veth end as the slave to the orchestrator bridge.
    '''
    ret = os.system('ip netns exec '+bridge_netns+ \
                   ' ip link set '+veth_end+' master '+bridge_name)

def _bring_up_if(netns, ifn):
    '''
    Bring up interface 'ifn' in network namespace 'netns'.
    '''
    ret = os.system('ip netns exec '+netns+ \
                   ' ip link set '+ifn+' up')

def _assign_addr(netns, ifn, addr):
    '''
    Assign network address to an interface inside a netns.
    '''
    ret = os.system('ip netns exec '+netns+ \
                   ' ip addr add '+addr+subnet_mask+' dev '+ifn)

def _def_route(netns, addr):
    '''
    Set an address as the default route in a network namespace.
    '''
    ret = os.system('ip netns exec '+netns+ \
                   ' ip route add default via '+addr)

def _add(packed_multiaddr):
    '''
    Add service details to net_map structure and create the namespaces.
    
    WARNING: no checking for the existence of the multiaddress is done in this
    module. This check is done the module/s which calls this.
    '''
    #TODO
    # error handle
    netns = _create_new_netns()
    _netns_resolvconf(netns)

    ret = _connect_netns_to_bridge(netns, packed_multiaddr)
    _net_map[packed_multiaddr] = (netns, packed_multiaddr)

    return None
 

def _del(multi_addr):
    '''
    Delete a netns instance (and the veth pair relating to it) from the net_map
    table.
    '''
    netns = ""
    for k in _net_map:
        if k == multi_addr:
            netns = _net_map[k][0]
            break

    if netns == "":
        return

    ret = os.system('ip netns del '+netns)
    del _net_map[multi_addr]

def _connect_netns_to_bridge(netns, \
                             packed_multiaddr):
    '''
    Function to connect a given network namespace to the
    orchestrator bridge.
    '''
    #TODO
    # error handle

    veth1, veth2 = _create_veth_pair()

    # move one veth end to the proper namespace
    _conn_ns_bridge(netns, veth1)
    # assign the other veth end to the bridge
    _slave_veth(veth2)

    # bring everything up
    _bring_up_if(bridge_netns, veth2)
    _bring_up_if(netns, veth1)
    _bring_up_if(netns, 'lo')

    # assign ip address to netns
    addr = ma.get_address(packed_multiaddr)
    _assign_addr(netns, veth1, addr)
    _def_route(netns, addr)

def _create_new_netns():
    '''
    Attempt to create a network namespace with a randomly generated
    string.
    '''
    name = _name()
    ret = os.system('ip netns add '+name)

    '''
    ASSUMPTION

    We will not be exhausting the random number range or even get remotely
    close to even 1/3 of the way. So not putting in any measures for stopping
    the recursion.
    '''
    if ret != 0:
        print("Netns already exists. Trying again.")
        name = _create_new_netns()
    return name

def _netns_resolvconf(netns):
    '''
    Each network namespace will now send dns requests to 
    '''
    ret = os.system('mkdir -p /etc/netns')
    with open('/etc/netns/'+netns, 'w') as f:
        f.write('nameserver '+ma.get_address(bridge_ipv4_multiaddr))

def _create_veth_pair():
    '''
    Attempt to create a veth pair with randomly generated strings.
    '''
    name1 = _name()
    name2 = _name()
    # create the veth pair in the bridge namespace so we can check for name
    # collisions
    
    '''
    ASSUMPTION

    Same assumption as in _create_new_netns.
    '''
    ret = os.system('ip netns exec '+bridge_netns+ \
                   ' ip link add '+name1+' type veth peer name '+name2)
    if ret != 0:
        print("Possible name collision. Don't know which name caused \
                the collision so clearing both and trying again.")
        os.system('ip netns exec '+bridge_netns+ \
                 ' ip link del '+name1)
        os.system('ip netns exec '+bridge_netns+ \
                 ' ip link del '+name2)
        name1, name2 = _create_veth_pair()
    return name1, name2

def instantiate_service(multi_addr):
    '''
    Params: unpacked multiaddress for the service to live on.
    '''
    #TODO
    # error handle
    ret = _add(ma.pack(multi_addr))
    return None

def shutdown_instance(multi_addr):
    '''
    Params: packed multiaddress of the service instance to delete
    '''
    _del(multi_addr)

def network_init():
    '''
    Return None if unable to complete.
    '''
    #TODO
    # error handle
    # some failures aren't critical and probably shoudl accommodate for that if
    # i have time
    print("Creating netns for the orchestrator.")
    ret = os.system('ip netns add '+bridge_netns)
    print("Creating the linux bridge.")
    ret = os.system('ip netns exec '+bridge_netns+' \
                    ip link add '+bridge_name+' type bridge')

    print("Adding IP address to bridge.")
    # assign IPv4 address
    _assign_addr(bridge_netns, \
                 bridge_name, \
                 ma.get_address(bridge_ipv4_multiaddr))

    # assign IPv6 address
    #ret = os.system('ip netns exec '+bridge_netns+' \
    #               ip address add '+ma.get_address(bridge_ipv6_multiaddr)+' \
    #               dev '+bridge_name)
    print("Setting the devices for the bridge netns to be up.")
    _bring_up_if(bridge_netns, 'lo')
    _bring_up_if(bridge_netns, bridge_name)

    print("Network namespace initialised.")
    print("Orchestrator bridge resides on "\
            +ma.get_address(bridge_ipv4_multiaddr)+subnet_mask)

    # initialise random number generator
    random.seed(time.time())

def cleanup():
    # only cleanup we need to do is clear all the network namespaces
    os.system('ip -all netns del')
