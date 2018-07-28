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
import service_handler as sh
import ipt_handler as ipt

_ipt_restore_file = "/tmp/sample_orchestrator_ipt.txt"
_ui_thread_name = "ui_thread"


def die_gracefully(signum, framenum):
    '''
    Perform cleanup.
    '''
    #TODO
    #clean up the system
    for thread in threading.enumerate():
        pass

    ipt.ipt_restore(ipt.restore_file)

#TODO
def ui_thread_handler():
    '''
    Listener for user interactions.
    '''
    helpString = '''\
Commands:
    h
        help
    q
        quit
    p servicetable
        print service table
    p serviceinstances
        print service instance table
    n serv_name
        new service instance with name `service_name`
    a serv_name multi_addr
        add new service instance with address ipv4_addr
    d serv_name
        delete an instance of serv_name
    r serv_name
        remove all instances of a service and stop tracking the service

Multiaddress
A multiaddress is an address of the format
/<protocol>/<address>/<protocol>/<address>/....

This experiment only supports IPv4 and IPv6 as it's not intended to run full
programs yet. The multiaddress implementation is home-brewed - the py-multiaddr
@ https://github.com/sbuss/py-multiaddr isn't used. Potentially if this
experiment needs to develop though, it will be.

eg. /ipv4/10.0.0.1
    /ipv6/fe80::8823:6dff:fee7:f172\
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
            ret = sh.new_service(s[1])
        elif s[0] == 'a':
            ret = sh.new_service_instance(s[1], s[2])
        elif s[0] == 'p':
            if len(s) == 1 or s[1] == "servicetable":
                print(sh.service_table_stringify())
            elif s[1] == "serviceinstances":
                print(sh.service_instances_stringify())
        elif s[0] == 'd':
            if len(s) < 2:
                print("What service do you want to delete an instance from.")
            else:
                sh.delete_service_instance(s[1])
        elif s[0] == 'r':
            if len(s) < 2:
                print("What service do you want to remove.")
            else:
                sh.remove_service(s[1])
        else:
            print("Command not recognised.")

        if ret != None:
            print(ret)



if __name__ == "__main__":
    if os.geteuid() != 0:
        print("This script needs sudo priveleges to run.")
        sys.exit(0)

    signal.signal(signal.SIGTERM, die_gracefully)
    signal.signal(signal.SIGINT, die_gracefully)

    # save the current iptables setting
    ipt.ipt_save()

    #TODO
    #set daemon value 
    #listen for user commands
    ui_thread = threading.Thread(name=_ui_thread_name, \
                                 target=ui_thread_handler,  \
                                 daemon=None)
    ui_thread.start()
