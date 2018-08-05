import os
from networking_handler import subnet_mask
'''
Module for dealing with operations on iptables

ramwan  <ray.wan@matrix.ai>
'''

_ipt_save = []

restore_file = "/tmp/matrix_prescramble_ipt.txt"
work_file = "/tmp/matrix_scramble_ipt.txt"

def ipt_save(_initial_save=False):
    for line in os.popen('iptables-save'):
        _ipt_save.append(line)

    if _initial_save:
        _flush_to_file(restore_file)
        _initial_save = False

def ipt_restore(target):
    '''
    Params: [ restore_file | work_file ]
    '''
    ret = os.popen('iptables-restore '+target)
    print("Original iptables settings restored.")

def _flush_to_file(target):
    with open(target, 'w') as f:
        for line in _ipt_save:
            f.write(line)

def create_nat_mapping(flow_id, source_ip, dest_ip):
    # NAT the flow id to the receiver service ip
    os.system('iptables -t nat -A OUTPUT -d '+flow_id+ \
              ' -j DNAT --to-destination '+dest_ip)
    # NAT the receiver service ip to the flow id
    os.system('iptables -t nat -A INPUT -s '+dest_ip+ \
              ' -d '+source_ip+ \
              ' -j SNAT --to-source '+flow_id)

def delete_nat_mapping(flow_id, source_ip, dest_ip):
    os.system('iptables -t nat -D OUTPUT -d '+flow_id+ \
              ' -j DNAT --to-destination '+dest_ip)
    os.system('iptables -t nat -D INPUT -s '+dest_ip+ \
              ' -d '+source_ip+ \
              ' -j SNAT --to-source '+flow_id)

def edit_nat_mapping(flow_id, new_source=None,\
                              old_source=None,\
                              new_dest=None,  \
                              old_dest=None):
    # delete and recreate
    delete_nat_mapping(flow_id, old_source, old_dest)
    create_nat_mapping(flow_id, new_source, new_dest)
