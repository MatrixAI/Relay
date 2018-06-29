# Tuntap script
# by ramwan
#
# Used to help me understand exactly how tuntap devices work.
# This bash script just creates a tun device and starts 2 python scripts
# which attack to the tap interface.

USER="ray"

sudo tunctl -n -u $USER
sudo ifconfig tun0 10.10.10.10 pointopoint 10.10.10.11 up
echo "Now run the python script in another terminal"
ping 10.10.10.11
