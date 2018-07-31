import networking_handler as nh
import multiaddress as ma
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
                                                                                
FlowID to IP mapping:                                                           
(key)           (value)                                                         
flowID          multiaddress
                                                                                
IP to FlowID mapping:                                                           
(key)           (value)                                                         
multiaddress    flowID
                                                                                
                                                                                
Note: flowID will also be an IP, but it will be a                               
reserved IP range in the usable address space of the physical machine           
'''
_service_table = {}
_service_instances = {}
_flow_id_to_ip = {}
_ip_to_flow_id = {}

def init_network():
    nh.network_init()


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

    instance_multiaddr = _service_table[serv_name][ len(_service_table[serv_name])-1 ]
    nh.shutdown_instance(instance_multiaddr)
    address = ma.get_address(instance_multiaddr)
    
    _delete_serv_instance_entry(address)
    _service_table[serv_name] = _service_table[serv_name][:-1]

def remove_service(serv_name):
    for i in range(len(_service_table[serv_name])):
        delete_service_instance(serv_name)

    del _service_table[serv_name]

def service_table_to_string():
    return str(_service_table)

'''
################################################################################
Service Instance Table operations
################################################################################
'''
def valid_service_instance(multi_addr):
    return ma.valid(multi_addr)

def serv_instance_exists(multi_addr):
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
