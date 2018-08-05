import networking_handler as nh
import multiaddress as ma
import ipt_handler as ih
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
    IP              [flowID, flowID...]

    Meaning by looking up a sender's IP in the `ip to flowid` table
    we can get a flowID which we then use to look up in the `flowid to ip`
    table to get the destination IP.
                                                                                
Note: flowID will also be an IP, but it will be a                               
reserved IP range in the usable address space of the physical machine           

Conntrack structure:
(key)           (value)
(IP, serv_name) flowID
'''

_FLOW_NOEXISTS = 0 # flow doesn't exist
_FLOW_EXISTS = 1 # flow good
_FLOW_DEAD = 2 # receiver is dead

_service_table = {}
_service_instances = {}
_flow_id_to_ip = {}
_ip_to_flow_id = {}
_conn_track = {}

# magic strings galore. should be fixed to be the upper ranges of
# the subnet which the bridge resides on
_flow_id_prefix = "10.0.254."
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

    packed_multiaddr = _service_table[serv_name][-1:][0]
    nh.shutdown_instance(packed_multiaddr)
    address = ma.get_address(packed_multiaddr)
    
    _delete_serv_instance_entry(address)
    _service_table[serv_name] = _service_table[serv_name][:-1]

    # delete any connection tracks which this service has initiated
    if ma.get_type(packed_multiaddr) == ma.IPV4:
        flows = _check_flow_existence('*', v4=address)

        if flows:
            for flow_id in flows:
                _del_flow(address, \
                          flow_id, \
                          _which_service_is_instance( \
                            ma.ip_to_packed_multiaddr( _flow_id_to_ip(flow_id) )\
                          ))

    elif ma.get_type(packed_multiaddr) == ma.IPV6:
        pass

def remove_service(serv_name):
    for i in range(len(_service_table[serv_name])):
        delete_service_instance(serv_name)

    del _service_table[serv_name]

def _get_random_instance(serv_name):
    if serv_name not in _service_table:
        return None
    return random.choice(_service_table[serv_name])

def _which_service_is_instance(packed_multiaddr):
    '''
    Assumption: service exists on the given multiaddr
    '''
    for service in _service_table:
        if packed_multiaddr in _service_table[service]:
            return service

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
    if ip:
        if ip in _service_instances:
            return True
    else:
        instance = ma.pack(multi_addr)
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
def get_flow_id(serv_name, v4=None, v6=None):
    '''
    Adds a flow entry to the flow bi-map. Checks for the existence of the a flow
    between the sender and receiver first. If a flow already exists then that
    flow id will be returned.

    Returns None on error

    NOTE:
    doesn't support one automaton needing to talk to multiple automatons of the
    same type.
    '''
    if v4:
        exists =  _check_flow_existence(serv_name, v4=v4)

        # if the flow already exists we return the same flow ID
        if exists == _FLOW_EXISTS:
            return _conn_flow_id( _conn_key(v4, serv_name) )

        # if the flow exists but the receiving server is dead, we need to
        # find a new receiver if there is one
        elif exists == _FLOW_DEAD:
            fid = _conn_flow_id( _conn_key(v4, serv_name) )
           # we don't need to call del() right now due to the implementation
            # of the flow table. However it probably would be good to call it to
            # make it clearer so I'm going to leave it here.
            #
            #_del_flow_id_to_ip(fid)
            #_del_conn( _conn_key(v4, serv_name) )

            instance = _get_random_instance(serv_name)
            if instance == None:
                return None

            instance_addr = ma.get_address(instance)
            
            # do iptables nat mappings
            with nh.Namespace( nh.get_ns_name( \
                                nh.ma.ip_to_packed_multiaddr(v4) \
                               )):
                ih.edit_nat_mapping(fid, new_source=v4,\
                                         old_source=v4,\
                                         new_dest=instance_addr,\
                                         old_dest=_flow_id_to_ip[fid])
            
            _add_flow_id_to_ip(instance_addr, fid)
            _add_conn(v4, serv_name, fid)

            return fid

        # create new flow
        elif exists == _FLOW_NOEXISTS:
            instance = _get_random_instance(serv_name)
            if instance == None:
                return None

            fid = _generate_flow_id()
            instance_addr = ma.get_address(instance)
            _add_ip_to_flow_id(v4, fid)
            _add_flow_id_to_ip(instance_addr, fid)

            # create connection tracking information
            _add_conn(v4, serv_name, fid)

            # do iptables nat mappings
            with nh.Namespace( nh.get_ns_name( \
                                ma.ip_to_packed_multiaddr(v4) \
                                )):
                ih.create_nat_mapping(fid, v4, instance_addr)

            return fid

    elif v6:
        # TODO
        return None

    return None

def _check_flow_existence(serv_name, v4=None, v6=None):
    '''
    Checks whether a flow already exists between the sender address and a
    service.

    If serv_name == '*' then the ip_to_flow_id mapping of the given address will
    be returned. Return type will be a list which could be empty.

    NOTE:
    We don't need to check for a reverse flow id because the receiver will send
    back to the same flow id that it received on.

    Need to find a way to yield flow ids other than having a service instance
    be deleted.
    '''
    if v4:
        if serv_name == '*':
            if v4 in _ip_to_flow_id:
                return _ip_to_flow_id[v4]
            return []

        return _conn_exists(v4, serv_name)
            #TODO
            # upon deletion of service, delete all conntrack and flow things
    if v6:
        # TODO
        return None
    else:
        return None
   
def _add_ip_to_flow_id(ip_addr, flow_id):
    if ip_addr not in _ip_to_flow_id:
        _ip_to_flow_id[ip_addr] = [flow_id]
        return
    _ip_to_flow_id[ip_addr].append(flow_id)

def _add_flow_id_to_ip(ip_addr, flow_id):
    _flow_id_to_ip[flow_id] = ip_addr

def _del_ip_to_flow_id(ip_addr, flow_id):
    if ip_addr in _ip_to_flow_id:
        del _ip_to_flow_id[ip_addr][ _ip_to_flow_id[ip_addr].index(flow_id) ]

def _del_flow_id_to_ip(flow_id):
    if flow_id in _flow_id_to_ip:
        del _flow_id_to_ip[flow_id]

def _del_flow(init_addr, flow_id, serv_name):
    '''
    Delete a flow initiated from init_addr to a service instance on a flow id
    '''
    with nh.Namespace( nh.get_ns_name( \
                        ma.ip_to_packed_multiaddr(v4) \
                     )): 
        ih.delete_nat_maping(flow_id, init_addr, _flow_id_to_ip[flow_id])

    _del_ip_to_flow_id(init_addr, flow_id)
    _del_flow_id_to_ip(flow_id)
    _free_flow_id(flow_id)
    conn = _conn_key(init_addr, serv_name)
    _del_conn(conn)

def _generate_flow_id():
    '''
    Generate a unique flow id and mark the value as taken.
    '''
    # TODO
    # IPv6 implementation
    # Assumption for IPv4: we won't have that many namespaces
    val = 0
    for i in range(255):
        if i not in _taken_flow_values:
            val = i
            break
    _taken_flow_values.append(val)
    return _flow_id_prefix+str(val)

def _free_flow_id(fid):
    '''
    IPv4 flow id ONLY
    '''
    # extract the last 
    val = fid[len(_flow_id_prefix):]
    del _taken_flow_values[ _taken_flow_values.index(val) ]

def ip_to_flow_stringify():
    return str(_ip_to_flow_id)

def flow_to_ip_stringify():
    return str(_flow_id_to_ip)

'''
################################################################################
Conntrack Operations
################################################################################
'''
def _conn_exists(sender_ip, dest_serv_name):
    conn = _conn_key(sender_ip, dest_serv_name)
    if conn in _conn_track:
        if serv_instance_exists(None, ip=_flow_id_to_ip[ _conn_flow_id(conn) ] ):
            return _FLOW_EXISTS
        else:
            return _FLOW_DEAD
    return _FLOW_NOEXISTS

def _conn_key(sender_ip, serv_name):
    '''
    Return a key value to be used with the _conn_track structure.
    '''
    return (sender_ip, serv_name)

def _conn_flow_id(conn_key):
    '''
    Return the flow id for the given connection.
    '''
    return _conn_track[conn_key]

def _add_conn(init_addr, serv_name, flow_id):
    '''
    Create a connection track for a service instance initiator, the service
    which it talks to and the flow_id which they communicate across.
    '''
    conn = _conn_key(init_addr, serv_name)
    _conn_track[ conn ] = flow_id

def _del_conn(conn_key):
    '''
    Delete a connection tracker from the table given it's key.
    '''
    if conn_key in _conn_track:
        del _conn_track[conn_key]
