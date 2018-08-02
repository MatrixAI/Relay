import socket
import service_flow_structures as sf
import multiaddress as ma
import threading
'''
Code heavily pulled from
https://github.com/pathes/fakedns/blob/master/fakedns.py
Cheers pathes

Modified as needed by ramwan <ray.wan@matrix.ai>
'''
_listening = False
dns_ipv4_addr=""
dns_header_length = 12

def listen():
    '''
    '''
    _listening = True
    dns_ipv4 = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    #dns_ipv6 = socket.socklet(socket.AF_INET6, socket.SOCK_DGRAM)

    #TODO
    # error handle
    dns_ipv4.bind(('', 53))
    #dns_ipv6.bind(('', 53))

    dns4_thread = threading.Thread(name="dns4_thread",     \
                                   target=_listen_thread,  \
                                   args=dns_ipv4)
    #dns6_thread = threading.Thread(name="dns6_thread",    \
    #                               target=_listen_thread, \
    #                               args=dns_ipv6)

    dns4_thread.start()
    #dns6_thread.start()

    dns4_thread.join()
    #dns6_thread.join()

    dns_ipv4.close()
    #dns_ipv6.close()

def _listen_thread(sock):
    while _listening:
        data, addr = sock.recv(1024)
        resp = _get_response(data)
        sock.sendto(resp, addr)

def stop_listen():
    '''
    '''
    _listening = False

def _get_response(request):
    '''
    On invalid requests or errors, return None.
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
        if question['qtype'] == b'\x00\x01' and \
                question['qclass'] == b'\x00\x01':
            accepted_questions.append(question)
        elif question['qtype'] == b'\x00\x1c' and \
                question['qclass'] == b'\x00\x01':
            accepted_question.append(question)

    response = (
        _response_header(data) +
        _response_questions(accepted_questions) +
        _response_answers(accepted_questions)
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
        print(question['name'])

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

def _response_answers(questions):
    '''
    Generates DNS response answers.
    See http://tools.ietf.org/html/rfc1035 4.1.3. Resource record format.
    '''
    records = b''

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
        if question['qtype'] == b'\x00\x01':
            _get_instance_addr(question['name'])
        # elif question['qtype'] == b'\x00\x1c'
        else:
            #TODO
            pass

        records += record

    return records

def _get_instance_addr(serv_name):
    return ma.get_address(sf.get_random_instance(serv_name))
