# Veth connectors from different namespaces to a bridge
# written by ramwan
# 
# A small script to set up 3 different network namespaces all connected by veth
# pair to a bridge residing in the host namespace. All namespaces are able to
# ping to each other.
# 
# IP1b          IP2b          IP3b
# +---+         +---+         +---+
# |NS1|         |NS2|         |NS3|
# +---+         +-+-+         +---+
#     |           |           |
#     |           |           |
#     +-----------+-----------+
#                 |
#               +-+--+
#               |host| bridge
#               +----+

NS1="A"
NS2="B"
NS3="C"
BRDG_NAME="BRIDGE"
NS1_default="10.10.10.1"
NS2_default="10.10.11.1"
NS3_default="10.10.12.1"
IP_BRDG="10.10.0.1/12"
IP1b="10.10.10.1/12"
IP2b="10.10.11.1/12"
IP3b="10.10.12.1/12"
VETH1a="VETH1a"
VETH1b="VETH1b"
VETH2a="VETH2a"
VETH2b="VETH2b"
VETH3a="VETH3a"
VETH3b="VETH3b"


function create() {
  sudo ip netns add $NS1
  sudo ip netns add $NS2
  sudo ip netns add $NS3

  sudo ip link add $VETH1a type veth peer name $VETH1b
  sudo ip link add $VETH2a type veth peer name $VETH2b
  sudo ip link add $VETH3a type veth peer name $VETH3b
  sudo ip link add $BRDG_NAME type bridge
  sudo ip link set dev $BRDG_NAME up

  sudo ip link set $VETH1a master $BRDG_NAME
  sudo ip link set $VETH1b netns $NS1
  sudo ip link set $VETH2a master $BRDG_NAME
  sudo ip link set $VETH2b netns $NS2
  sudo ip link set $VETH3a master $BRDG_NAME
  sudo ip link set $VETH3b netns $NS3

  sudo ip addr add $IP_BRDG dev $BRDG_NAME
  sudo ip netns exec $NS1 ip addr add $IP1b dev $VETH1b
  sudo ip netns exec $NS2 ip addr add $IP2b dev $VETH2b
  sudo ip netns exec $NS3 ip addr add $IP3b dev $VETH3b

  sudo ip link set dev $VETH1a up
  sudo ip link set dev $VETH2a up
  sudo ip link set dev $VETH3a up
  sudo ip netns exec $NS1 ip link set dev lo up
  sudo ip netns exec $NS1 ip link set dev $VETH1b up
  sudo ip netns exec $NS2 ip link set dev lo up
  sudo ip netns exec $NS2 ip link set dev $VETH2b up
  sudo ip netns exec $NS3 ip link set dev lo up
  sudo ip netns exec $NS3 ip link set dev $VETH3b up

  sudo ip netns exec $NS1 ip route add default via $NS1_default
  sudo ip netns exec $NS2 ip route add default via $NS2_default
  sudo ip netns exec $NS3 ip route add default via $NS3_default

  echo "IP1b          IP2b          IP3b
+---+         +---+         +---+
|NS1|         |NS2|         |NS3|
+---+         +-+-+         +---+
    |           |           |
    |           |           |
    +-----------+-----------+
                |
              +-+--+
              |host| bridge
              +----+"
  echo "IP1b := $IP1b"
  echo "IP2b := $IP2b"
  echo "IP3b := $IP3b"
  echo "BR_IP:= $IP_BRDG"
}

function clean() {
  sudo ip netns delete $NS1
  sudo ip netns delete $NS2
  sudo ip netns delete $NS3
  sudo ip link delete $BRDG_NAME
}

if [ "$1" == "create" ]; then
  create
elif [ "$1" == "clean" ]; then
  clean
else
  echo "Usage: ./bridge_veth_ns.sh [ create | clean ]"
fi
