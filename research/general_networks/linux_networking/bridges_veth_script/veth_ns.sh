# Veth pairs in linux network namesepaces
# written by ramwan
#
# 2 namespaces are created, NS1 and NS2. 2 veth pairs are created as well with 
# one end attached to a namespace and the other attached to the global
# namespace. IP addresses are assigned with both namespaces being on separate
# logical networks.
#
#     IP1b      IP1a +------+ IP2a        IP2b
# +---+              |      |                +---+
# |NS1+==============+ HOST +================+NS2|
# +---+              |      |                +---+
#                    +------+
#           Veth and namespace setup
#
# All IPs are able to be pinged from everywhere.
# Extra note: both ends of the veth pair need to be on the same subnet.

NS1="A"
NS2="B"
VETH1a="veth1a"
VETH1b="veth1b"
VETH2a="veth2a"
VETH2b="veth2b"
NS1_default="10.10.10.2"
NS2_default="10.10.11.2"
IP1a="10.10.10.1/24"
IP1b="10.10.10.2/24"
IP2a="10.10.11.1/24"
IP2b="10.10.11.2/24"

function create() {
  sudo ip netns add $NS1
  sudo ip netns add $NS2
  
  sudo ip link add $VETH1a type veth peer name $VETH1b
  sudo ip link add $VETH2a type veth peer name $VETH2b
  
  sudo ip link set $VETH1b netns $NS1
  sudo ip link set $VETH2b netns $NS2
  
  sudo ip addr add $IP1a dev $VETH1a
  sudo ip link set dev $VETH1a up
  sudo ip addr add $IP2a dev $VETH2a
  sudo ip link set dev $VETH2a up
  sudo ip netns exec $NS1 ip addr add $IP1b dev $VETH1b
  sudo ip netns exec $NS1 ip link set dev $VETH1b up
  sudo ip netns exec $NS2 ip addr add $IP2b dev $VETH2b
  sudo ip netns exec $NS2 ip link set dev $VETH2b up

  sudo ip netns exec $NS1 ip link set dev lo up
  sudo ip netns exec $NS2 ip link set dev lo up
  
  sudo ip netns exec $NS1 ip route add default via $NS1_default
  sudo ip netns exec $NS2 ip route add default via $NS2_default

  echo "     IP1b      IP1a +------+ IP2a        IP2b
 +---+              |      |                +---+
 |NS1+==============+ HOST +================+NS2|
 +---+              |      |                +---+
                    +------+
           Veth and namespace setup"
  echo ""
  echo "IP1b := ${IP1b}"
  echo "IP1a := ${IP1a}"
  echo "IP2b := ${IP2b}"
  echo "IP2a := ${IP2a}"

  echo "NS1 := ${NS1}"
  echo "NS2 := ${NS2}"
}

function clean() {
  sudo ip netns del $NS1
  sudo ip netns del $NS2
}


if [ "$1" == "create" ]; then
  create
elif [ "$1" == "clean" ]; then
  clean
else
  echo "Usage: ./veth_ns.sh [ create | clean ]"
fi
