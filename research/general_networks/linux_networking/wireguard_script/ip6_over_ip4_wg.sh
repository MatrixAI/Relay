#!/bin/bash

NS1="A"
NS2="B"
NS3="C"
NS4="D"
A_6addr=fd00::1/64
B_4addr=10.0.0.1/24
C_4addr=10.0.0.2/24
D_6addr=fd00::4/64
BC_6addr=fc00::1/64
CB_6addr=fc00::2/64
WG1="w6guardA"
WG4="w6guardD"
V1="v1"
V2="v2"

function conf1() {
  echo "[Interface]
#Address = fc00::1/64
ListenPort = 51820
PrivateKey = 8OvpW0vAQhv1hROUVlWl+uYo+3gg5zKA9za7mnaiPVQ=
#SaveConfig = false
#Table = off

[Peer]
PublicKey = /bJqFLQ9fr4pIQYxtYBcjvu6RfivRe2EvxLk9xWldXc=
AllowedIPs = ::/0
  " > ./w6guardA.conf
}

function conf2() {
  echo "[Interface]
#Address = fc00::2/64
ListenPort = 51821
PrivateKey = OEoHe9Roxfjrcve/p0iyPXAms8Q7qOzbh3Y2e03550M=
#SaveConfig = false
#Table = off

[Peer]
PublicKey = YkE9YVB8NCiR6HDRXIcOOgHpVPmHOjTD+DxwcL7TzX0=
AllowedIPs = ::/0
Endpoint = [fc00::1]:51820
PersistentKeepalive = 2
  " > ./w6guardD.conf
}

function create() {
  conf1
  conf2

  ip netns add $NS1
  ip netns add $NS2
  ip netns add $NS3
  ip netns add $NS4

  # set up lo devices and wireguard devices
  ip -n $NS2 link add $WG1 type wireguard
  ip -n $NS3 link add $WG4 type wireguard
  ip -n $NS1 link set lo up
  ip -n $NS2 link set lo up
  ip -n $NS3 link set lo up
  ip -n $NS4 link set lo up

  ip -n $NS2 link set $WG1 netns $NS1
  ip -n $NS3 link set $WG4 netns $NS4

  ip netns exec $NS1 wg setconf $WG1 ./w6guardA.conf
  ip netns exec $NS4 wg setconf $WG4 ./w6guardD.conf

  # set up tunnels and veth connections between NS2 and NS3
  ip -n $NS2 tunnel add "BC" mode sit \
    remote `echo ${C_4addr} | sed -e 's/\/.*$//'` \
    local `echo ${B_4addr} | sed -e 's/\/.*$//'`
  ip -n $NS3 tunnel add "CB" mode sit \
    remote `echo ${B_4addr} | sed -e 's/\/.*$//'` \
    local `echo ${C_4addr} | sed -e 's/\/.*$//'`

  ip -n $NS2 addr add ${BC_6addr} dev "BC"
  ip -n $NS3 addr add ${CB_6addr} dev "CB"

  ip -n $NS2 link add $V1 type veth peer name $V2
  ip -n $NS2 link set $V2 netns $NS3
  ip -n $NS2 addr add ${B_4addr} dev $V1
  ip -n $NS3 addr add ${C_4addr} dev $V2
  ip -n $NS2 link set $V1 up
  ip -n $NS3 link set $V2 up

  # turn on devices and finish assigning addresses
  ip -n $NS2 link set "BC" up
  ip -n $NS3 link set "CB" up
  ip -n $NS1 link set $WG1 up
  ip -n $NS4 link set $WG4 up

  ip -n $NS1 addr add ${A_6addr} dev $WG1
#  ip -n $NS1 addr add fe80::1/64 dev $WG1
  ip -n $NS4 addr add ${D_6addr} dev $WG4
#  ip -n $NS4 addr add fe80::2/64 dev $WG4
  ip -n $NS1 -6 route add default dev $WG1
  ip -n $NS4 -6 route add default dev $WG4

  ip -n $NS2 route add fc00::2 dev "BC"
  ip -n $NS3 route add fc00::2 dev lo
  
  ip -n $NS3 route add fc00::1 dev "CB"
  ip -n $NS2 route add fc00::1 dev lo
}

function clean() {
  ip netns del $NS1
  ip netns del $NS2
  ip netns del $NS3
  ip netns del $NS4
}

if [[ $EUID -ne 0 ]]; then
  echo "You need to sudo this."
  exit
fi

if [[ "$1" == "create" ]]; then
  create
elif [[ "$1" == "clean" ]]; then
  clean
else
  echo "Usage: bash ip6_over_ip4_wg.sh [create | clean]"
fi
