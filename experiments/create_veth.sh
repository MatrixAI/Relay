# create_veth.sh
# by ramwan
#
# Takes 2 veth names and creates a veth pair.

sudo ip link add $1 type veth peer name $2
