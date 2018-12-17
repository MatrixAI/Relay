#!/usr/bin/python3

'''
Parser for config files to be evoked from a shell script.
It parses a single config (given as an argument) and saves relevant output to
a tmp file (path provided as an argument).

Usage: python3 parseConfig.py \
                config_path tmp_dir keys_dir ifnet ifname ifport ifaddr

ramwan <ray@matrix.ai>
'''

import sys
import os
import re

def get_my_pubkey(keys_dir, ifname):
    key = ""
    with open(keys_dir+'/'+ifname+'.pub', 'r') as f:
        key = f.readline().strip()
    return key

conf_path = sys.argv[1]
keys_dir  = sys.argv[2]
ifnet     = sys.argv[3]
ifname    = sys.argv[4]
ifaddr    = sys.argv[5]
ifport    = sys.argv[6]


is_name    = re.compile('^#[^ ]+$')
match_addr = re.compile('^#Address = ')
match_tabl = re.compile('^#Table = ')
#match_kliv = re.compile('^PersistentKeepalive = ')
match_newp = re.compile('^ *\[Peer\] *$')

# of the format [ip6_endpoint]:port
my_address = "["+ifaddr+"]:"+ifport
my_pubkey  = get_my_pubkey(keys_dir, ifname)
curr_peer  = ""
match      = []


with open(conf_path, 'r') as f:
    for line in f:
        line = line.strip()
        # match start of new block
        if re.match(is_name, line):
            curr_peer = re.sub(r'^#', '', line).lower()
            continue

        # if not reading our own config
        if curr_peer != ifname:
            if re.match(match_newp, line): 
                # I believe that persistent-keepalive won't be reset when we
                # change other attributes.... but will need to see in an actual
                # test
                cmd = "ip netns exec "+ifnet+" wg set "+ifname+\
                      " peer "+my_pubkey+" endpoint "+my_address
                print(cmd)
