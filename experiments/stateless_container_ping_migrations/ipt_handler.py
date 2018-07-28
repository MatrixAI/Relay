import os
'''
Module for dealing with operations on iptables

ramwan  <ray.wan@matrix.ai>
'''

_initial_save = True
_ipt_save = []

restore_file = "/tmp/matrix_prescramble_ipt.txt"
work_file = "/tmp/matrix_scramble_ipt.txt"

def ipt_save():
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
    if ret != None:
        print("Unable to perform iptables restore.")
        print(os.strerror(ret))

def _flush_to_file(target):
    with open(target, 'w') as f:
        for line in _ipt_save:
            f.write(line)
