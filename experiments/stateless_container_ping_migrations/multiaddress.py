import socket
'''
Simple multiaddress module.

ramwan  <ray.wan@matrix.ai>
'''

'''
Multiaddress datastructure

For now very simple:
    multiaddr = ( "protocol", "address", ... )
'''

_supported_protocols = [\
        "ipv4",\
        "ipv6"\
]

IPV4 = "ipv4"
IPV6 = "ipv6"

def valid(multi_addr):
    '''
    Check whether multi_addr is valid.

    Returns True/False
    '''
    s = multi_addr[1:].split('/')
    
    # very bootleg implementation. works as needed
    # for now. aka for situations
    # /ipv4/10.0.0.1
    # /ipv6/fe80::8823:6dff:fee7:f172
    if s[0] in _supported_protocols:
        if s[0] == "ipv4":
            try:
                return socket.inet_pton(socket.AF_INET, s[1])
            except:
                return False
        else:
            try:
                return socket.inet_pton(socket.AF_INET6, s[1])
            except:
                return False
    return False

def pack(multi_addr):
    '''
    Precondition: multi_addr has been checked and is valid

    Converts a multiaddress to a datastructure documented
    at the top of this file. 
    
    Returns the datastructure.
    '''
    s = multi_addr[1:].split('/')
    return (s[0], s[1])

def get_address(packed_multiaddr):
    '''
    Returns the netowrk layer address in the multiaddress structure.
    Assumes that the multiaddr doesn't support anything below IP in the TCP/IP
    stack.
    '''
    return packed_multiaddr[1]

def get_type(packed_multiaddr):
    return packed_multiaddr[0]

def ip_to_multiaddr(string_ip):
    try:
        socket.inet_pton(socket.AF_INET, string_ip)
        return "/ipv4/"+string_ip
    except:
        return "/ipv6/"+string_ip
    return None

def ip_to_packed_multiaddr(string_ip):
    return pack( ip_to_multiaddr(string_ip) )
        
