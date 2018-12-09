#!/bin/bash

# simulating ip6-to-ip4-to-ip6 transition of ipv6 overlay networks running on
# and ip4 public internet
#
# Setup:
# [a 6]==[b 6->4]==[c 4->6]==[d 6]

NS1="A"
NS2="B"
NS3="C"
NS4="D"
A_6addr=2002::1/64
B_6addr=2002::2/64
B_4addr=10.1.0.1/24
C_4addr=10.1.0.2/24
C_6addr=2003::2/64
D_6addr=2003::1/64
BC_6addr=fd00::1/64
CB_6addr=fd00::2/64
A_6="v1"
B_6="v2"
B_4="v3"
C_4="v4"
C_6="v5"
D_6="v6"

function create()
{
  ip netns add ${NS1}
  ip netns add ${NS2}
  ip netns add ${NS3}
  ip netns add ${NS4}

  ip -n $NS1 link add $A_6 type veth peer name $B_6
  ip -n $NS2 link add $B_4 type veth peer name $C_4
  ip -n $NS3 link add $C_6 type veth peer name $D_6 

  ip -n $NS1 link set dev $B_6 netns $NS2
  ip -n $NS2 link set dev $C_4 netns $NS3
  ip -n $NS3 link set dev $D_6 netns $NS4

  # each ipv6 node has to have a few things, defined in
  # rfc4291
  ip -n $NS1 addr add ${A_6addr} dev $A_6
  ip -n $NS2 addr add ${B_6addr} dev $B_6
  ip -n $NS2 addr add ${B_4addr} dev $B_4
  ip -n $NS3 addr add ${C_4addr} dev $C_4
  ip -n $NS3 addr add ${C_6addr} dev $C_6
  ip -n $NS4 addr add ${D_6addr} dev $D_6

  ip -n $NS2 tunnel add "BC" mode sit \
    remote `echo $C_4addr | sed -e 's/\/.*$//'` \
    local `echo $B_4addr | sed -e 's/\/.*$//'`
  ip -n $NS3 tunnel add "CB" mode sit \
    remote `echo $B_4addr | sed -e 's/\/.*$//'` \
    local `echo $C_4addr | sed -e 's/\/.*$//'`

  ip -n $NS2 addr add ${BC_6addr} dev "BC"
  ip -n $NS3 addr add ${CB_6addr} dev "CB"

  ip -n $NS1 link set dev lo up
  ip -n $NS2 link set dev lo up
  ip -n $NS3 link set dev lo up
  ip -n $NS4 link set dev lo up

  ip -n $NS1 link set dev $A_6 up
  ip -n $NS2 link set dev $B_6 up
  ip -n $NS2 link set dev $B_4 up
  ip -n $NS3 link set dev $C_4 up
  ip -n $NS3 link set dev $C_6 up
  ip -n $NS4 link set dev $D_6 up
  ip -n $NS2 link set dev "BC" up
  ip -n $NS3 link set dev "CB" up

  ip -n $NS1 -6 route add default dev $A_6
  ip -n $NS4 -6 route add default dev $D_6
  ip -n $NS2 -6 route add fd00::1 dev $B_6
  ip -n $NS2 -6 route add fd00::4 dev "BC"
  ip -n $NS3 -6 route add fd00::4 dev $C_6
  ip -n $NS3 -6 route add fd00::1 dev "CB"

  echo "Links set up."
}

function clean()
{
  ip netns del $NS1
  ip netns del $NS2
  ip netns del $NS3
  ip netns del $NS4
}

if [[ $EUID -ne 0 ]]; then
  echo "Must be root to run this."
  exit
fi

if [[ $1 == "create" ]]; then
  create
elif [[ $1 == "clean" ]]; then
  clean
else
  echo "Usage: bash sit_tunnel_example.sh [create | clean]"
fi
