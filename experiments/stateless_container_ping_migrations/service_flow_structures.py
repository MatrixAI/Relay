import networking_handler as nh
import multiaddress as ma
import random
import time
'''
Data structures and operations on service and flow tables.

ramwan  <ray.wan@matrix.ai>
'''

'''                                                                             
- DATA STRUCTURES -                                                             
                                                                                
Service table structure:                                                        
(key)           (value)                                                         
Service name    [multiaddress, multiaddress...]                                        
                                                                                
Service instance structure:                                                     
(key)           (value)                                                         
Address         multiaddress

Flow Table Abstraction:
(key)       (value)     (key)
send_IP   flow_ID     recv_IP

Implemented as 2 separate dictionaries:
    FlowID to IP mapping:                                                           
    (key)           (value)                                                         
    flowID          IP
                                                                                
    IP to FlowID mapping:                                                           
    (key)           (value)                                                         
    (IP, flowID)    flowID

    Meaning by looking up a sender's IP in the `ip to flowid` table
    we can get a flowID which we then use to look up in the `flowid to ip`
    table to get the destination IP.
                                                                                
Note: flowID will also be an IP, but it will be a                               
reserved IP range in the usable address space of the physical machine           

Conntrack structure:
(key)           (value)
(IP, serv_name) flowID
'''
_FLOW_EXISTS = 0 # flow good
_FLOW_DEAD = 0 # receiver is dead
_FLOW_NOEXISTS = 0 # flow doesn't exist

_service_table = {}
_service_instances = {}
_flow_id_to_ip = {}
_ip_to_flow_id = {}
_conn_track = {}

# magic strings galore. should be fixed to be the upper ranges of
# the subnet which the bridge resides on
_flow_id_prefix = "10.0.255."
_taken_flow_values = []

random.seed(time.time())

def init_network():
    nh.network_init()

def restore_network():
    nh.cleanup()

'''
################################################################################
Service Table operations
################################################################################
'''
def serv_exists(serv_name):
    if serv_name in _service_table:
        return True
    return False

def add_service(serv_name):
    _service_table[serv_name] = []

def add_service_instance(serv_name, multi_addr):
    '''
    Precondition: checks on serv_name existence and
                  multi_addr validity have been performed
    '''
    packed_multiaddr = ma.pack(multi_addr)
    _service_table[serv_name].append(packed_multiaddr)
    _add_serv_instance_entry(ma.get_address(packed_multiaddr), multi_addr)
    #TODO
    # error handle
    ret = nh.instantiate_service(multi_addr)

def delete_service_instance(serv_name):
    '''
    Precondition: check on serv_name existence has been performed
    
    Deletes the last entry in the list of services. This could change
    later to a random entry or something else.
    '''
    if len(_service_table[serv_name]) == 0:
        return

    packed_multiaddr = _service_table[serv_name][ len(_service_table[serv_name])-1 ]
    nh.shutdown_instance(packed_multiaddr)
    address = ma.get_address(packed_multiaddr)
    
    _delete_serv_instance_entry(address)
    _service_table[serv_name] = _service_table[serv_name][:-1]

def remove_service(serv_name):
    for i in range(len(_service_table[serv_name])):
        delete_service_instance(serv_name)

    del _service_table[serv_name]

def _get_random_instance(serv_name):
    if serv_name not in _service_table:
        return None
    return random.choice(_service_table[serv_name])

def service_table_to_string():
    return str(_service_table)

'''
################################################################################
Service Instance Table operations
################################################################################
'''
def valid_service_instance(multi_addr):
    return ma.valid(multi_addr)

def serv_instance_exists(multi_addr, ip=None):
    instance = ma.pack(multi_addr)
    if ip:
        if ip in _service_instances:
            return True
    else:
        if ma.get_address(instance) in _service_instances:
            return True
    return False

def _add_serv_instance_entry(key, packed_multiaddr):
    '''
    Params: the key of the service instance (the address)
    '''
    _service_instances[key] = packed_multiaddr

def _delete_serv_instance_entry(key):
    '''
    Params: the key of the service instance (the address)
    '''
    del _service_instances[key]

def service_instances_to_string():
    return str(_service_instances)

'''
################################################################################
Flow table operations
################################################################################
'''
def add_flow_entry(serv_name, v4=None, v6=None):
    '''
    Adds a flow entry to the flow bi-map. Checks for the existence of the a flow
    between the sender and receiver first. If a flow already exists then that
    flow id will be returned.

    NOTE:
    doesn't support one automaton needing to talk to multiple automatons of the
    same type.
    '''
    if v4:
        # if the flow already exists we return the same flow ID
        if _check_flow_existence(serv_name, v4=v4):
            return _ip_to_flow_id[v4]

        f_id = _generate_flow_id()
        _add_ip_to_flow_id(v4, f_id)
        # TODO
        _add_flow_id_to_ip(recv_instance_addr, f_id)

        return f_id
    elif v6:
        # TODO
        return None
    else:
        return None

def _check_flow_existence(serv_name, v4=None, v6=None):
    '''
    Checks whether a flow already exists between the sender and the receiver
    addresses.

    NOTE:
    We don't need to check for a reverse flow id because the receiver will send
    back to the same flow id that it received on.

    Need to find a way to yield flow ids other than having a service instance
    be deleted.
    '''
    if v4:
        fid = ""

        exists = _conn_exists(v4, serv_name)
        if exists == FLOW_EXISTS:
            return _get_conn_flow_id(v4, serv_name)
        elif exists == FLOW_DEAD:
            fid = _get_conn_flow_id(v4, serv_name)
            _del_flow_id_to_ip(fid)
            
            instance = _get_random_instance(serv_name)
            if instance == None:
                return None

            instance_addr = ma.get_address(instance)
            _add_flow_id_to_ip(instance_addr, fid)

            return fid
        elif exists == FLOW_NOEXISTS:
            fid = _generate_flow_id()

            #TODO
            # upon deletion of service, delete all conntrack and flow things

        return False
    if v6:
        # TODO
        return None
    else:
        return None
   
def _add_ip_to_flow_id(ip_addr, flow_id):
    _ip_to_flow_id[ip_addr].append(flow_id)

def _add_flow_id_to_ip(ip_addr, flow_id):
    _flow_id_to_ip[flow_id] = ip_addr

def _del_ip_to_flow_id(ip_addr, flow_id):
    del _ip_to_flow_id[ip_addr][ _ip_to_flow_id[ip_addr].index(flow_id) ]

def _del_flow_id_to_ip(flow_id):
    del _flow_id_to_ip[flow_id]

def _generate_flow_id():
    '''
    Generate a unique flow id.
    '''
    # TODO
    # IPv6 implementation
    # Assumption for IPv4: we won't have 122 namespaces
    val = 0
    for i in range(255):
        if i not in _taken_flow_values:
            val = i
            break
    return _flow_id_prefix+str(val)

'''
################################################################################
Conntrack Operations
################################################################################
'''
def _conn_exists(sender_ip, dest_serv_name):
    conn = (sender_ip, dest_serv_name)
    if conn in _conn_track:
        if _serv_instance_exists( _flow_id_to_ip[ _conn_track[conn] ] ):
            return FLOW_EXISTS
        else:
            return FLOW_DEAD
    return FLOW_NOEXISTS

def _add_conn():
    pass

def _del_conn():
    pass

def _get_conn_flow_id(sender_ip, dest_serv_name):
    '''
    Assumption: connection exists
    '''
    conn = (sender_ip, dest_serv_name)
    for ip in _ip_to_flow_id:
        if _flow_id_to_ip[ _ip_to_flow_id[ip] ] == \
           _flow_id_to_ip[ _conn_track[conn] ]:
            return _ip_to_flow_id[ip]
