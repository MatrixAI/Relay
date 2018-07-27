__service_ip_index__ = 0

'''
Service table structure:

(key)           (value)
Service name    [(IP:port),(IP:port)...]
'''
serviceTable = {}
iptSave = []

def __serv_exists__(serv_name):
    if serv_name in serviceTable:
        return True
    return False

def newService(serv_name):
    '''
    Add a new service name to the service table.
    
    Returns None on success.
    '''
    if serv_name not in serviceTable:
        serviceTable[serv_name] = []
        return None
    return serv_name+" already exists."

#TODO
# check for valid port
# check for valid IP
# check whether the IP is already used
def newServiceInstance(serv_name, ip, port=None):
    '''
    Add a new ip address and port to a service in the service table.

    Returns None on success.
    '''
    if serv_name not in serviceTable:
        return "Service '"+serv_name+"' doesn't exists."
    
    for elem in serviceTable[serv_name]:
        if elem[__service_ip_index__] == ip:
            return "ip address for this service already exists."

    serviceTable[serv_name].append([ip, port])
    return None
