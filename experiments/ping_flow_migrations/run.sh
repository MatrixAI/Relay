#!/usr/bin/bash
DEST="C"

if [[ $EUID -ne 0 ]]; then
  echo "Need to be root."
  exit 1
fi

ip netns del A 2> /dev/null
ip netns del B 2> /dev/null
ip netns del C 2> /dev/null
ip netns del D 2> /dev/null

ip netns add A
ip netns add B
ip netns add C
ip netns add D

ip -n B link add vA type veth peer name vAB
ip -n B link add vC type veth peer name vCB
ip -n B link add vD type veth peer name vDB
ip -n B link set dev vA netns A
ip -n B link set dev vC netns C
ip -n B link set dev vD netns D
ip -n A link set dev lo up
ip -n A link set dev vA up
ip -n B link set dev lo up
ip -n B link set dev vAB up
ip -n B link set dev vCB up
ip -n B link set dev vDB up
ip -n C link set dev vC up
ip -n D link set dev vD up

ip -n A address add fc00::1/64 dev vA
ip -n C address add fc01::1/64 dev vC
ip -n D address add fc02::1/64 dev vD
ip -n B address add fc00::2/64 dev vAB
ip -n B address add fc01::2/64 dev vCB
ip -n B address add fc02::2/64 dev vDB

ip netns exec B sysctl net.ipv6.conf.all.forwarding=1

ip -n A -6 route add default dev vA via fc00::2
ip -n C -6 route add default dev vC via fc01::2
ip -n D -6 route add default dev vD via fc02::2

ip netns exec B ip6tables -t nat -A POSTROUTING -s fc00::1 -j SNAT \
  --to-source fe00::1
ip netns exec B ip6tables -t nat -A PREROUTING -d fe00::1 -j DNAT \
  --to-destination fc01::1

echo "Netns A now connects to netns C on abstract address fe00::1"

while read line; do
  if [[ "$line" -eq "swap" ]]; then
    # swap flow to netns D
    if [[ "$DEST" == "C" ]]; then
      ip netns exec B ip6tables -t nat -D PREROUTING -d fe00::1 -j DNAT \
        --to-destination fc01::1
      ip netns exec B ip6tables -t nat -A PREROUTING -d fe00::1 -j DNAT \
        --to-destination fc02::1
      DEST="D"
      ip netns exec B conntrack -F
      echo "Netns A now connects to netns D on abstract address fe00::1"
      continue
    fi
    # swap flow to netns C
    if [[ "$DEST" == "D" ]]; then
      ip netns exec B ip6tables -t nat -D PREROUTING -d fe00::1 -j DNAT \
        --to-destination fc02::1
      ip netns exec B ip6tables -t nat -A PREROUTING -d fe00::1 -j DNAT \
        --to-destination fc01::1
      DEST="C"
      ip netns exec B conntrack -F
      echo "Netns A now connects to netns C on abstract address fe00::1"
      continue
    fi
  else
    echo "I only accept \"swap\" >:("
  fi
done

ip netns del A
ip netns del B
ip netns del C
ip netns del D
