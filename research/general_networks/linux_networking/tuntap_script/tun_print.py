'''
Code grabbed from
https://gist.github.com/glacjay/585369/b229da72a0dc84dd27d12afc5b76d0c5c44bb9c3

modified by ramwan

This snippet just prints to STDOUT the raw IP packets that the tun interface
receives.
'''
import os
import fcntl
import struct
import subprocess

# C standard library constants
# They're all defined in /linux/if_tun.h
# but are still just magic numbers. idk what they mean.
TUNSETIFF = 0x400454ca
TUNSETOWNER = TUNSETIFF + 2
IFF_TUN = 0x0001
IFF_TAP = 0x0002
IFF_NO_PI = 0x1000

# Open file corresponding to the TUN device.
tun = open('/dev/net/tun', 'r+b')
ifr = struct.pack('16sH', 'tun0', IFF_TUN | IFF_NO_PI)
fcntl.ioctl(tun, TUNSETIFF, ifr)
fcntl.ioctl(tun, TUNSETOWNER, 1000)

while True:
  # Read an IP packet been sent to this TUN device.
  # This just prints it out to the screen
  packet = list(os.read(tun.fileno(), 2048))
  print(packet)
