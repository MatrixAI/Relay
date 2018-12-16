#!/usr/bin/python3
import sys
import os

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
| fc00 |     Host Hash     | listen port |          automaton hash            |
+-----------------------------------------------------------------------------+
                                128 bits

FLOW ADDRESS
A flow address in an abstract address representing a persistent peer that an
automaton will be able to talk to. The automaton will send packets to the same
flow address regardless of whether the peer is on the same real address or not.
This address is part of the unique-local range of ipv6 (fc00::/7). This differs
from a Real Address in that it starts with "fd00" rather than "fc00".

   8            40                        16                      64
+-----------------------------------------------------------------------------+
|fd00| initiator host hash | initiator listen port |   target automaton hash  |
+-----------------------------------------------------------------------------+
                                128 bits

For both REAL and FLOW addresses, if the provided host and automaton hashes are
greater than the specified lengths, the higher order bits will be taken. If the
given hashes are below specified lengths, the higher order bits will be padded
with 0s.

RETURN CODE of NOT 0 indicates error.
'''

if len(sys.argv) != 5:
    sys.exit(1)
if sys.argv[4] < 1024 or sys.argv[4] > 0xFFFF:
    sys.exit(1)

# Get address type
address_type = ""
if sys.argv[1] == "flow":
    address_type = "fd00"
elif sys.argv[1] == "real":
    address_type = "fc00"

# Get host bits
host = sys.argv[3]
bytes_in_host = 40/8
if len(host) < bytes_in_host:
    host = host.rjust( bytes_in_host - len(host), '0')
elif len(host) > bytes_in_host:
    host = host[0:bytes_in_host]

# Get port bits
port = int(sys.argv[4])

# Get automaton hash bits
a_hash = sys.argv[5]
bytes_in_ahash = 64/8
if len(a_hash) < bytes_in_ahash:
    a_hash = a_hash.rjust( bytes_in_ahash - len(a_hash ), '0')
elif len(a_hash) > bytes_in_ahash:
    a_hash = a_hash[0:bytes_in_ahash]

address = address_type + host + port + a_hash

