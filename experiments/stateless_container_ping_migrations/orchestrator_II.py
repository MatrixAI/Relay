#!/bin/python3
'''
Sample orchestrator written in Python3 in order to demonstrate possibilities
of service migration and to see what kinds of issues we will hit.

ramwan  <ray.wan@matrix.ai>
'''

import sys
import os
import threading
import signal
import subprocess
import service_handler as sh

serviceTable = sh.serviceTable
iptSave = sh.iptSave

__original_ipt_settings__ = []
__ipt_restore_file__ = "/tmp/sample_orchestrator_ipt.txt"
__ui_thread_name__ = "ui_thread"


def dieGracefully(signum, framenum):
    '''
    Perform cleanup.
    '''
    #TODO
    # restore original IPT settings
    # and clean up the system
    for thread in threading.enumerate():
        pass

def flushIptToFile():
    with open(__ipt_restore_file__, 'w') as f:
        for line in iptSave:
            f.write(line)

def callIptRestore():
    ret = os.popen('iptables-restore '+__ipt_restore_file__).close()
    if ret != None:
        print("Unable to perform iptables restore. Got error code "+ret)

#TODO
def uiThreadHandler():
    '''
    Listener for user interactions.
    '''
    helpString = '''\
Commands:
    h
        help
    q
        quit
    p
        print service table
    n serv_name
        new service instance with name `service_name`
    a serv_name ipv4_addr
        add new service instance with address ipv4_addr
    d serv_name
        delete an instance of serv_name
    r serv_name
        remove all instances of a service and stop tracking the service
    '''



    # user input loop
    # TODO
    # check validity of provided arguments
    while True:
        ret = None
        s = input().split(' ')
        
        if len(s) == 0:
            continue

        if s[0] == 'h':
            print(helpString)
        elif s[0] == 'q':
            #TODO
            # call dieGracefully
            print("Ctrl+D to exit. soz.")
        elif s[0] == 'n':
            ret = sh.newService(s[1])
        elif s[0] == 'a':
            # TODO
            # doesn't make sense to NAT to a specific port.
            # automatons will know which port they need to talk to
            # maybe?
            ret = sh.newServiceInstance(s[1], s[2], port=None)
        elif s[0] == 'p':
            print(serviceTable)
        elif s[0] == 'd':
            #TODO
            print("TODO")
        elif s[0] == 'r':
            #TODO
            print("TODO")
        else:
            print("Command not recognised.")

        if ret != None:
            print(ret)



if __name__ == "__main__":
    if os.geteuid() != 0:
        print("This script needs sudo priveleges to run.")
        sys.exit(0)

    signal.signal(signal.SIGTERM, dieGracefully)
    signal.signal(signal.SIGINT, dieGracefully)

    # save the current iptables setting
    for line in (os.popen('iptables-save')):
        iptSave.append(line)
    __original_ipt_settings = deepcopy(iptSave)
    flushIptToFile()

    #listen for user commands
    uiThread = threading.Thread(name=__ui_thread_name__, \
                                target=uiThreadHandler,  \
                                daemon=None)
    uiThread.start()
