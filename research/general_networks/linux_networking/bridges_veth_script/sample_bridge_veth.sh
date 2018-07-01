# Veth pairs and bridge script
# by ramwan
# 
# To be able to run cleanup.sh, source this file (. sample_bridge_veth.sh)
# help -> http://www.opencloudblog.com/?p=66
#
# This script will set up a 2 new namespace and link it them to a linux bridge.
# Adding a phsyical ethernet device to the bridge is commented out because I
# don't have a cable connection when writing this.
# 
# NOTE: we are unable to bridge wireless interfaces
# https://superuser.com/questions/597834/bridging-wifi-to-ethernet-on-
# ubuntu-not-working
# 
# The structure that is created is as follows:
#
# Namespace A                                    Namespace B
#      ns1 ------------> BRIDGE <------------------ ns2
#                          ^
#                          |
#                         main
#
# From the main namespace, we're able to ping ns1 and ns2 but we can't ping
# anywhere except the bridge from nsA and nsB
# https://sreeninet.wordpress.com/2016/05/29/macvlan-and-ipvlan/https://sreeninet.wordpress.com/2016/05/29/macvlan-and-ipvlan/
# In the above article, there's a line explaining possibly why pinging from ns1
# to ns2 won't work:
# 'With Bridge, it is needed to use NAT for external connectivity.'
# Does this mean in the scenario of a packet going from ns1->bridge->ns2, ns2
# will see the source IP as the bridge's IP?
# 
# More thoughts on the above problem :=
# since the packets travelling from ns1 to ns2 will need to go through the host
# namespace as the bridge resides there, perhaps we actually need to add rules
# to iptables. My default policy for the FORWARD chain is DROP. This may explain
# why no pings were going through despite knowledge of the network addresses.

#INTERFACE='enp0s25'
export NS_NAME1="A"
export NS_NAME2="B"
export BRIDGE_NAME="bridge"
export VETH_0n="veth0n" # namespace
export VETH_0b="veth0b" # bridge
export VETH_1n="veth1n"
export VETH_1b="veth1b"
export VETH_2n="veth2n"
export VETH_2b="veth2b"

NETWORK_IP="10.10.10.0/24"
BRIDGE_IP="10.10.10.1/24"
IP_ADDR_MAIN="10.10.10.10/24"
IP_ADDR_NS1="10.10.10.20/24"
IP_ADDR_NS2="10.10.10.30/24"

# create a new network namespace
echo "Creating new namespaces"
sudo ip netns add $NS_NAME1
sudo ip netns add $NS_NAME2

# create a bridge in the global namespace
echo "Creating bridge and assigning address"
sudo ip link add name $BRIDGE_NAME type bridge
sudo ip link set $BRIDGE_NAME up
sudo ip addr add "$BRIDGE_IP" dev $BRIDGE_NAME
sudo ip route flush dev $BRIDGE_NAME
sudo ip route add "$NETWORK_IP" dev $BRIDGE_NAME

# create a veth pair in the global namespace
echo "Creating veth pairs"
# sudo ip link add name $VETH_0n type veth peer name $VETH_0b
sudo ip link add name $VETH_1n type veth peer name $VETH_1b
sudo ip link add name $VETH_2n type veth peer name $VETH_2b
# sudo ip link set $VETH_0n up
sudo ip link set $VETH_1n up
sudo ip link set $VETH_2n up

# connect VETH_0, VETH_2, VETH_4 to the bridge
echo "Connecting veth pairs to bridge"
# sudo brctl addif $BRIDGE_NAME $VETH_0b
sudo brctl addif $BRIDGE_NAME $VETH_1b
sudo brctl addif $BRIDGE_NAME $VETH_2b

# bring up the eth device and link it to the bridge
# if you're connected to a dhcp server, you'll have an IP assigned to this
# interface already
# sudo ip link set $INTERFACE up
# sudo ip link set $INTERFACE master $BRIDGE_NAME

# send one of the veth ends to the other namespace
echo "Moving veth ends to namespaces"
sudo ip link set $VETH_1n netns $NS_NAME1
sudo ip netns exec $NS_NAME1 ifconfig $VETH_1n "$IP_ADDR_NS1" up
sudo ip link set $VETH_2n netns $NS_NAME2
sudo ip netns exec $NS_NAME2 ifconfig $VETH_2n "$IP_ADDR_NS2" up

# sudo ifconfig $VETH_0n "$IP_ADDR_MAIN" up
