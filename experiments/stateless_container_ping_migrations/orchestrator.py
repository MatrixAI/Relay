'''
Simple sample orchestrator for experiment
written by ramwan
'''

import socket
import sys
import os
import struct
import threading
import binascii
import time
import signal

__arp_listen__ = False
__service_listen__ = False

MESSAGE_SIZE = 8
REGISTER_SERVICE = 'R'
REGISTER_EXPECT_RESPONSE = 'E'
UNREGISTER_SERVICE = 'U'
MESSAGE_FORMAT = "cchd"

'''
Service table structure:

(key)           (value)
Service Name    Addresses
    A           [(IP,port), (IP,port)....]
'''
serviceTable = {}
_listenAddress_ = "127.0.0.1"
_listenPort_ = 8000
# we will need to get the network interface and 
# MAC address of the interface via environment variables I think
_networkInterface_ = "wlp3s0"
_interfaceMACAddress_ = "0xaaaaaaaaaaaa"


'''
'''
def dieGracefully():
    print("Shutting down gracefully.")
    for thread in threading.enumerate():
        if thread.name == "ArpThread":
            __arp_listen__ = False
        elif thread.name == "ServiceThread":
            __service_listen__ = False

'''
returns a C struct type of format
{announcement, serviceName, port, address}

announcement: REGISTER_SERVICE | EXPECT_RESPONSE | UNREGISTER_SERVICE
serviceName: char
address: string IPv4 address
port: port service listens on
'''
def createMessage(announcement, serviceName,\
                address, port):
    return struct.pack(MESSAGE_FORMAT,\
                        announcement,\
                        serviceName,\
                        socket.htons(port),\
                        socket.htonl(socket.inet_aton(address)))

'''
returns nothing

takes in a raw bytes array as the message
either adds the service to the service table or removes it
depending on the message received
'''
def parseMessage(message):
    announcement, serviceName, port, address = \
                                    struct.unpack(MESSAGE_FORMAT, message)
    
    addressTuple = (socket.inet_ntoa(socket.ntohl(address)),\
                    socket.ntohs(port))

    if announcement == REGISTER_SERVICE:
        if serviceName in serviceTable:
            serviceTable[ serviceName ].append(addressTuple)
        else:
            serviceTable[ serviceName ] = [addressTuple]
    elif announcement == UNREGISTER_SERVICE:
        if serviceName in serviceTable:
            indexOf = serviceTable[ serviceName ].index(addressTuple)
            del serviceTable[ serviceName ][ indexOf ]
        else:
            pass
    elif announcement == REGISTER_EXPECT_RESPONSE:
        #TODO
        pass

'''
Function which listens for ARP broadcasts and sends the first initial
ARP broadcast to announce machine presence.

This is how we announce the presence of a new machine on the network -
the joining machine does a gratuitious ARP broadcast with its IP. The receivers
of this packet then send out a 
'''
def arpHandler(arpSock):
    __arp_listen__ = True

    # on startup, send gratuitious ARP broadcast
    broadcastAddress = 0xffffffffffff
    arpSock.

    while __arp_listen__:
        # listen for ARP broadcasts
        packet = arpSock.recvfrom(2048)
        etherHeader = packet[0][0:14]
        etherUnpacked = struct.unpack("!6s6s2s", ethernetHeader)
    
        # if it's not an ARP packet we don't care
        if etherUnpacked[2].decode('utf-8') != '\x08\x06':
            continue
        
        arpHeader = packet[0][14:42]
        arpDetailed = struct.unpack("2s2s1s1s2s6s4s6s4s", arpHeader)
    
        print("******************_ARP_HEADER_******************")
        print("Source IP:       ", socket.inet_ntoa(arp_detailed[6]))
        print("Dest MAC:        ", binascii.hexlify(arp_detailed[7]))
        print("*************************************************\n")


'''
'''
def serviceHandler(servSock):
    __service_listen__ = True
    while __service_listen__:
        parsemessage(servSock.recv(1024))

if __name__ == "__main__":
    signal.signal(signal.SIG_TERM, dieGracefully)
    signal.signal(signal.SIG_INT, dieGracefully)

    # initialise service message parsing socket
    servS = socket.socket(socket.AF_INET, \
            socket.SOCK_DGRAM, 0)
    servS.bind((_listenAddress_, _listenPort_))
    
    # create ethernet socket to broadcast and receive gratuitious ARPs
    # 0x0806 is the protocol number for ARP. Here we can only capture inbound
    # ARP packets but that's all we need. If we want to capture all packets, we
    # need to swap 0x0806 to 0x0003
    arpS = socket.socket(socket.AF_PACKET, \
                    socket.SOCK_RAW, socket.htons(0x0003))
    arpS.bind((_networkInterface_, 0))


    # create and start threads
    arpThread = threading.Thread(target=arpHandler,\
                                 name="ArpThread",\
                                 args=(arpS))
    servThread = threading.Thread(target=serviceHandler,\
                                  name="ServiceThread",
                                  args=(servS))

    arpThread.start()
    servThread.start()

    arpThread.join()
    servThread.join()
