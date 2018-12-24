#/bin/bash

if [[ $EUD -ne 0 ]]; then
  echo "Need to be root."
  exit
fi

ip -all netns del
ip netns add A
ip netns add B
ip netns add C

ip -n B link add vA_nets type veth peer name vA_br
ip -n B link add vC_nets type veth peer name vC_br
ip -n B link add BRIDGE type bridge

ip -n B link set dev vA_nets netns A
ip -n B link set dev vC_nets netns C

ip -n A address add fc00::2/64 dev vA_nets
ip -n C address add fc00::3/64 dev vC_nets
ip -n B address add fc00::1/64 dev BRIDGE
ip -n B link set vA_br master BRIDGE
ip -n B link set vC_br master BRIDGE

ip -n A link set lo up
ip -n A link set vA_nets up
ip -n B link set lo up
ip -n B link set BRIDGE up
ip -n B link set vA_br up
ip -n B link set vC_br up
ip -n C link set lo up
ip -n C link set vC_nets up

ip -n A -6 route add default dev vA_nets
ip -n C -6 route add default dev vC_nets
