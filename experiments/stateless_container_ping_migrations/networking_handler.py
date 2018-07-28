import os
import multiaddress as ma
'''
Module for interacting with the linux networking stack and iptables itself.

ramwan  <ray.wan@matrix.ai>
'''

'''
- OVERALL STRUCTURE -

Each 'service' will be running in a separate network namespace connected by
a veth pair to a linux bridge running in its own network namespace. 
'''

'''
- DATA STRUCTURES -

'''

_bridge_exists = False

_bridge_netns = "orchestrator"
_bridge_name = "the_bridge"
_bridge_ipv4_multiaddr = ""
_bridge_ipv6_multiaddr = ""

def _create_new_netns():
    pass

def _create_veth_pair():
    pass

def instantiate_service():
    pass

def network_init():
    '''
    Return None if unable to complete.
    '''
    #TODO
    # perform cleanups upon failure to instantiate
    if _bridge_exists:
        return None

    ret = os.popen('ip netns add '+_bridge_netns)
    if ret != None:
        print("Unable to create bridge netns.")
        print(os.strerror(ret))
        return None
    
    ret = os.popen('ip netns exec '+_bridge_netns+' \
                    ip link add '+_bridge_name+' type bridge')
    if ret != None:
        print("Unable to add bridge to a separate netns.")
        print(os.strerror(ret))
        return None

    # assign IPv4 address
    ret = os.popen('ip netns exec '+_bridge_netns+' \
                    ip address add '+ma.get_address(_bridge_ipv4_multiaddr)+' \
                    dev '+_bridge_name)
    if ret != None:
        print("Unable to assign "+ma.get_address(_bridge_ipv4_multiaddr)+" \
                to bridge.")
        print(os.strerror(ret))
        return None

    # assign IPv6 address
    ret = os.popen('ip netns exec '+_bridge_netns+' \
                    ip address add '+ma.get_address(_bridge_ipv6_multiaddr)+' \
                    dev '+_bridge_name)
    if ret != None:
        print("Unable to assign "+ma.get_address(_bridge_ipv6_multiaddr)+" \
                to bridge.")
        print(os.strerror(ret))
        return None

    ret = os.popen('ip netns exec '+_bridge_netns+' \
                    ip link set dev lo up')
    ret = os.popen('ip netns exec '+_bridge_netns+' \
                    ip link set dev '+_bridge_name+' up')
    if ret != None:
        print("Unable to bring the bridge up.")
        print(os.strerror(ret))
        return None

    return 0
