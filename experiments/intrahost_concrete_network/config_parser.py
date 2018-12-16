#!/usr/bin/python3

'''
Parser for config files to be evoked from a shell script.
It parses a single config (given as an argument) and saves relevant output to
a tmp file (path provided as an argument).

Usage: python3 parseConfig.py \
                config_path tmp_dir keys_dir hostnet ifnet ifname ifport

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

def write_to_file(fpath, mode, string):
    with open(fpath, mode) as f:
        f.write(string)

conf_path = sys.argv[1]
tmp_dir   = sys.argv[2]
keys_dir  = sys.argv[3]
hostnet   = sys.argv[4]
ifnet     = sys.argv[5]
ifname    = sys.argv[6]
ifport    = sys.argv[7]


is_name    = re.compile('^#[^ ]+$')
match_addr = re.compile('^#Address = ')
match_tabl = re.compile('^#Table = ')
match_kliv = re.compile('^PersistentKeepalive = ')
match_newp = re.compile('^ *[Peer] *$')

# of the format [ip6_endpoint]:port
my_address = ""
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

        # if reading our own config
        if curr_peer == ifname:
            if re.match(match_addr, line):
                addr = re.sub(r'^#[ ]?Address = ', '', line)
                addr = re.sub(r'/[0-9]+$', '', addr)
                my_address = "["+addr+"]:"+ifport
                continue


            # table handling not implemented
            if re.match(match_tabl, line):
                continue

        # if reading peer entry
        else:
            if re.match(match_newp, line): 
                # I believe that persistent-keepalive won't be reset when we
                # change other attributes.... but will need to see in an actual
                # test
                cmd = "ip netns exec "+ifnet+" wg set "+ifname+\
                      " peer "+my_pubkey+" endpoint "+my_address
                write_to_file(tmp_dir+'/'+curr_peer, 'a', cmd)
