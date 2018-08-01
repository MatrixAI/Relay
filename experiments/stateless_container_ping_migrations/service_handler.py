import service_flow_structures as sf
'''
Interface into service and flow table data structures and oeprations.

ramwan <ray.wan@matrix.ai>
'''
def init_network():
    sf.init_network()

def restore_network():
    sf.restore_network()

def new_service(serv_name):
    '''
    Add a new service name to the service table.
    
    Returns None on success.
    '''
    if not sf.serv_exists(serv_name):
        sf.add_service(serv_name)
        return None
    return serv_name+" already exists."

#TODO
# check for valid port
# check for valid IP
# check whether the IP is already used
def new_service_instance(serv_name, multi_addr):
    '''
    Add a new ip address and port to a service in the service table.

    Returns None on success.
    '''
    if not sf.serv_exists(serv_name):
        return "Service '"+serv_name+"' doesn't exists."
    
    if not sf.valid_service_instance(multi_addr):
        return "Multiaddress "+multi_addr+" isn't valid."

    if sf.serv_instance_exists(multi_addr):
        return "Multiaddress "+multi_addr+" already has a server on it."

    sf.add_service_instance(serv_name, multi_addr)
    return None

def service_table_stringify():
    '''
    Returns a stringified verion of the service table.
    '''
    return sf.service_table_to_string()

def service_instances_stringify():
    '''
    Returns a stringified version of the service isntances table.
    '''
    return sf.service_instances_to_string()

def delete_service_instance(serv_name):
    '''
    Deletes a service instance. Always succeeds.
    '''
    if not sf.serv_exists(serv_name):
        return

    sf.delete_service_instance(serv_name)

def remove_service(serv_name):
    '''
    Remove a service and delete all of its instances.
    '''
    sf.remove_service(serv_name)
