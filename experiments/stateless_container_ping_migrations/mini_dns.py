import socket
import multiaddress as ma
import threading
import os
import select
import time
from networking_handler import bridge_netns, bridge_ipv4_multiaddr, Namespace
import service_flow_structures as sf

'''
DNS Code heavily pulled from
https://github.com/pathes/fakedns/blob/master/fakedns.py
Cheers pathes

Modified as needed by ramwan <ray.wan@matrix.ai>
'''

'''
Variable definitions
'''
_dns_active = False
_dns_ipv4 = None
#_dns_ipv6 = None
_dns4_thread = None
#_dns6_thread = None
dns_ipv4_addr = ma.get_address(bridge_ipv4_multiaddr)
#dns_ipv6_addr = ma.get_address(bridge_ipv6_multiaddr)
dns_header_length = 12

def listen():
    '''
    Initialises sockets in the orchestrator/bridge netns and starts separate
    threads to listen on them.
    '''
    global _dns_active
    global _dns_ipv4
    #global dns_ipv6
    global _dns4_thread
    #global _dns6_thread
    _dns_active = True

    with Namespace(bridge_netns):
        _dns_ipv4 = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
        #_dns_ipv6 = socket.socket(socket.AF_INET6, socket.SOCK_DGRAM)

        _dns_ipv4.bind((dns_ipv4_addr, 53))
        #_dns_ipv6.bind((dns_ipv6_addr,53))

        #TODO
        # error handle
        _dns4_thread = threading.Thread(name="dns4_thread",     \
                                       target=_listen,  \
                                       args=[_dns_ipv4])
        #_dns6_thread = threading.Thread(name="dns6_thread",    \
        #                               target=_listen, \
        #                               args=[_dns_ipv6])

        _dns4_thread.start()
        #dns6_thread.start()

def _listen(sock):
    '''
    Function called by threads which poll on sockets.
    '''
    poll = select.poll()
    poll.register(sock, select.POLLIN)

    # TODO
    # for some reason, _get_response() will return None for a few tries
    # before meaningful data is returned even if a proper response is possible.
    # for example repeatedly requesting for a flowID that exists will usually
    # (perhaps 6/7? tries on avg) return results instantly but occassionaly will
    # return None.
    #
    # Hacky fix with try/except.
    while _dns_active:
        events = poll.poll(500)

        if events:
            if events[0][1] & select.POLLIN:
                data, addr = sock.recvfrom(1024)
                resp = _get_response(data, addr)
                try:
                    sock.sendto(resp, addr)
                except Exception as e:
                    continue
    #shutdown
    poll.unregister(sock)

def stop_listen():
    '''
    Stop and close the DNS sockets.
    '''
    global _dns_active
    _dns_active = False

    _dns4_thread.join()
    #_dns6_thread.join()

    os.close(_dns_ipv4.fileno())
    #os.close(_dns_ipv6.fileno())
    print("DNS threads stopped.")

def _get_response(request, addr):
    '''
    On invalid requests or errors, return None.
    `addr` is the inet_pton() of a string network address
    '''
    data = request.strip()

    # If request doesn't even contain full header, don't respond.
    if len(data) < dns_header_length:
        return None

    # Try to read questions - if they're invalid, don't respond.
    try:
        all_questions = _extract_questions(data)
    except IndexError:
        return None

    # Accept IPIv6 and IPv4 requests
    accepted_questions = []
    for question in all_questions:
        # IPv4 or IPv6
        if question['qtype'] == b'\x00\x01' or \
           question['qtype'] == b'\x00\x1c' and \
           question['qclass'] == b'\x00\x01':
            accepted_questions.append(question)

    response = (
        _response_header(data) +
        _response_questions(accepted_questions) +
        _response_answers(accepted_questions, addr)
    )

    return response

def _extract_questions(data):
    '''
    Extracts question section from DNS request data.
    See http://tools.ietf.org/html/rfc1035 4.1.2. Question section format.
    '''
    questions = []
    # Get number of questions from header's QDCOUNT
    n = (data[4] << 8) + data[5]
    # Where we actually read in data? Start at beginning of question sections.
    pointer = dns_header_length
    # Read each question section
    for i in range(n):
        question = {
            'name': [],
            'qtype': '',
            'qclass': '',
        }
        length = data[pointer]

        # Read each label from QNAME part
        while length != 0:
            start = pointer + 1
            end = pointer + length + 1
            question['name'].append(data[start:end])
            pointer += length + 1
            length = data[pointer]
        # Read QTYPE
        question['qtype'] = data[pointer+1:pointer+3]
        # Read QCLASS
        question['qclass'] = data[pointer+3:pointer+5]
        # Move pointer 5 octets further (zero length octet, QTYPE, QNAME)
        pointer += 5
        questions.append(question)

    return questions

def _response_header(data):
    '''
    Generates DNS response header.
    See http://tools.ietf.org/html/rfc1035 4.1.1. Header section format.
    '''
    header = b''

    # ID - copy it from request
    header += data[:2]
    # QR     1    response
    # OPCODE 0000 standard query
    # AA     0    not authoritative
    # TC     0    not truncated
    # RD     0    recursion not desired
    # RA     0    recursion not available
    # Z      000  unused
    # RCODE  0000 no error condition
    header += b'\x80\x00'
    # QDCOUNT - question entries count, set to QDCOUNT from request
    header += data[4:6]
    # ANCOUNT - answer records count, set to QDCOUNT from request
    header += data[4:6]
    # NSCOUNT - authority records count, set to 0
    header += b'\x00\x00'
    # ARCOUNT - additional records count, set to 0
    header += b'\x00\x00'

    return header

def _response_questions(questions):
    '''
    Generates DNS response questions.
    See http://tools.ietf.org/html/rfc1035 4.1.2. Question section format.
    '''
    # TODO
    # TEST FOR IPv6
    sections = b''

    for question in questions:
        section = b''
        for label in question['name']:
            # Length octet
            section += bytes([len(label)])
            section += label
        # Zero length octet
        section += b'\x00'
        section += question['qtype']
        section += question['qclass']
        sections += section

    return sections

def _response_answers(questions, addr):
    '''
    Generates DNS response answers.
    See http://tools.ietf.org/html/rfc1035 4.1.3. Resource record format.
    '''
    # TODO
    # Check for the existence of an instance of the requested server.
    # If none exist, we should return a response that indicates there is no
    # IP mapping for that domain name request.

    '''
    for name in names:
        try get an address
        
        if None, response indicating no server
        else regular response as below
    '''

    records = b''
    names = _join_question_names(questions)

    count = 0
    for question in questions:
        record = b''
        for label in question['name']:
            # Length octet
            record += bytes([len(label)])
            record += label
        # Zero length octet
        record += b'\x00'
        # TYPE - just copy QTYPE
        # TODO QTYPE values set is superset of TYPE values set, handle different QTYPEs, see RFC 1035 3.2.3.
        record += question['qtype']
        # CLASS - just copy QCLASS
        # TODO QCLASS values set is superset of CLASS values set, handle at least * QCLASS, see RFC 1035 3.2.5.
        record += question['qclass']
        # TTL - 32 bit unsigned integer. Set to 0 to inform, that response
        # should not be cached.
        record += b'\x00\x00\x00\x00'
        # RDLENGTH - 16 bit unsigned integer, length of RDATA field.
        # In case of QTYPE=A and QCLASS=IN, RDLENGTH=4.
        record += b'\x00\x04'
        # RDATA - in case of QTYPE=A and QCLASS=IN, it's IPv4 address.

        # TODO
        # handle names without any instances
        if question['qtype'] == b'\x00\x01':
            addr = _get_flow_id(names[count], addr)
            record += socket.inet_aton(addr)
        # elif question['qtype'] == b'\x00\x1c'
        else:
            #TODO
            pass
        count += 1

        records += record

    return records

def _join_question_names(split_questions):
    '''
    DNS request formatting has each label of the domain name split up so we need
    to join it back together in order to look up the ip.
    '''
    names = []
    for i in range(len(split_questions)):
        names.append([])
        for j in range(len(split_questions[i]['name'])):
            names[i].append(split_questions[i]['name'][j].decode('utf-8'))
        names[i] = '.'.join(names[i])
    return names

def _get_flow_id(serv_name, sender_addr):
    '''
    Returns a flow ID which the DNS request sender will send requests to.
    Represents the flow abstraction so that requests can transparently be
    migrated.

    Adds entry to the flow table.
    Behaviour of add_flow_entry -> if flow exists, we'll just get the same flow
    id back.
    '''
    fid = None
    #try:
    socket.inet_pton(socket.AF_INET, sender_addr[0])
    fid = sf.get_flow_id(serv_name, \
                             v4=sender_addr[0])
    #except Exception as e:
    #    print(e)
    #    # ipv6 address
    #    fid = sf.get_flow_id(serv_name, \
    #                         v6=sender_addr[0])

    return fid
