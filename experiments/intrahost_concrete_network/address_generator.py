#!/usr/bin/python3
import sys
import binascii
import ipaddress

'''
Script which takes parameters and generates string ipv6 addresses based upon the
specification described below.

Usage: python3 address_generator.py [flow | real] host port automaton_hash

REAL ADDRESS
A real address is an address that is given to an interface. It's the address
which the interface will listen on and send from. This address is part of the
unique-local range of ipv6 (fc00::/7).

   8            40                16                      64
+-----------------------------------------------------------------------------+
| fc   |     Host Hash     | listen port |          automaton hash            |
+-----------------------------------------------------------------------------+
                                128 bits

FLOW ADDRESS
A flow address in an abstract address representing a persistent peer that an
automaton will be able to talk to. The automaton will send packets to the same
flow address regardless of whether the peer is on the same real address or not.
This address is part of the unique-local range of ipv6 (fc00::/7). This differs
from a Real Address in that it starts with "fd00" rather than "fc00". We have
the host hash and listen port as the initiator's values because the receiving
endpoint can change but this address should persist.

   8            40                        16                      64
+-----------------------------------------------------------------------------+
|fd  | initiator host hash | initiator listen port |   target automaton hash  |
+-----------------------------------------------------------------------------+
                                128 bits

For both REAL and FLOW addresses, if the provided host and automaton hashes are
greater than the specified lengths, the higher order bits will be taken. If the
given hashes are below specified lengths, the higher order bits will be padded
with 0s.

RETURN CODE of NOT 0 indicates error.
'''
if len(sys.argv) < 5:
    print("Not enough arguments.")
    sys.exit(1)

TYPE_LEN  = 8
HOST_LEN  = 40
PORT_LEN  = 16
AHASH_LEN = 64


address_type = sys.argv[1]
host_hash = int(sys.argv[2], 16)
listen_port = int(sys.argv[3])
automaton_hash = int(sys.argv[4], 16)

address = 0
'''
SANITY CHECKS
'''

# check address type
if address_type == "flow":
    address_type = 0xfd
elif address_type == "real":
    address_type = 0xfc
else:
    print("Address type unknown")
    sys.exit(1)

# check host bits
max_val = 2**39 # 40bits
for i in range(39):
    tmp = max_val >> i
    max_val |= tmp

while host_hash > max_val:
    host_hash >>= 1

if listen_port <= 1024 or listen_port > 65535:
    print("Port is incorrect")
    sys.exit(1)

# check automaton hash bits
max_val = 2**63 # 64 bits
for i in range(63):
    tmp = max_val >> i
    max_val |= tmp

while automaton_hash > max_val:
    automaton_hash >>= 1

'''
Piece it all together
'''
address |= address_type << (128-8)
address |= host_hash << (120-40)
address |= listen_port << (80-16)
address |= automaton_hash

print(ipaddress.IPv6Address(address))
