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

bridge_netns_exists = False
bridge_exists = False

bridge_netns = "orchestrator"
bridge_name = "the_bridge"
bridge_ipv4_multiaddr = ma.pack("/ipv4/10.10.10.10")
bridge_ipv6_multiaddr = "/ipv6/ff:ff::"

def _name():
    '''
    Return a random string.
    '''
    seed = random.randint(-10000, 10000)
    return base64.b64encode( str(seed).encode('utf-8') ).decode('utf-8')

def _add(packed_multiaddr):
    '''
    Add service details to net_map structure and create the namespaces.
    
    WARNING: no checking for the existence of the multiaddress is done in this
    module. This check is done the module/s which calls this.
    '''
    #TODO
    # error handle
    netns = _create_new_netns()
    veth = _create_veth_pair()

    ret = _connect_netns_to_bridge(netns, veth[0], veth[1], packed_multiaddr)
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
                             veth1, \
                             veth2, \
                             packed_multiaddr):
    '''
    '''
    #TODO
    # error handle

    # move one veth end to the proper namespace
    ret = os.system('ip netns exec '+bridge_netns+ \
                   ' ip link set '+veth1+' netns '+ netns)
    # assign the other veth end to the bridge
    ret = os.system('ip netns exec '+bridge_netns+ \
                   ' ip link set '+veth2+' master '+bridge_name)

    # bring everything up
    ret = os.system('ip netns exec '+bridge_netns+ \
                   ' ip link set '+veth2+' up')
    ret = os.system('ip netns exec '+netns+ \
                   ' ip link set '+veth1+' up')
    ret = os.system('ip netns exec '+netns+ \
                   ' ip link set lo up')


    # assign ip address to netns
    addr = ma.get_address(packed_multiaddr)
    ret = os.system('ip netns exec '+netns+ \
                   ' ip addr add '+addr+' dev '+veth1)
    ret = os.system('ip netns exec '+netns+ \
                   ' ip route add default via '+addr)

def _create_new_netns():
    '''
    Attempt to create a network namespace with a randomly generated
    string.

    Returns None if everything proceeds smoothly.
    '''
    name = _name()
    ret = os.system('ip netns add '+name)
    return name

def _create_veth_pair():
    '''
    Attempt to create a veth pair with randomly generated strings.

    Returns None if everything proceeds smoothly.
    '''
    name1 = _name()
    name2 = _name()
    # create the veth pair in the bridge namespace so we can check for name
    # collisions
    ret = os.system('ip netns exec '+bridge_netns+ \
                   ' ip link add '+name1+' type veth peer name '+name2)
    return (name1, name2)

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

    ret = os.system('ip netns add '+bridge_netns)
    bridge_netns_exists = True
    
    ret = os.system('ip netns exec '+bridge_netns+' \
                    ip link add '+bridge_name+' type bridge')
    bridge_exists = True

    # assign IPv4 address
    ret = os.system('ip netns exec '+bridge_netns+' \
                    ip address add '+ma.get_address(bridge_ipv4_multiaddr)+' \
                    dev '+bridge_name)

    # assign IPv6 address
    #ret = os.system('ip netns exec '+bridge_netns+' \
    #               ip address add '+ma.get_address(bridge_ipv6_multiaddr)+' \
    #               dev '+bridge_name)

    ret = os.system('ip netns exec '+bridge_netns+' \
                    ip link set dev lo up')
    
    ret = os.system('ip netns exec '+bridge_netns+' \
                    ip link set dev '+bridge_name+' up')

    print("Network namespaces initialised.")

    # initialise random number generator
    random.seed(time.time())

    return 0

def cleanup():
    os.system('ip netns delete '+bridge_netns)
    os.system('ip -all netns del')
