#/bin/bash

NS1="A"
NS2="B"
NS3="C"
WG1="wg1"
WG2="wg2"
WG3="wg3"

function wg1_conf() {
  echo "[Interface]
  #Address = fc00::1/64
  PrivateKey = 8OvpW0vAQhv1hROUVlWl+uYo+3gg5zKA9za7mnaiPVQ=
  ListenPort = 51820

  [Peer]
  # wg3
  PublicKey = ULjvnWG/dSkSZL9VjIErrZCPoZI9H8+7FvvLkANVfEg=
  AllowedIPs = ::/0
  PersistentKeepalive = 3
  Endpoint = [::1]:51822
  " > ~/wireguard/wg1.conf
}

function wg3_conf() {
  echo "[Interface]
  #Address = fc00::2/64
  PrivateKey = aKUS0H1LcdJ1Ut4dq4SrnZApZU7HPp24/QNADrxvIFs=
  ListenPort = 51822
  
  [Peer]
  # wg1
  PublicKey = YkE9YVB8NCiR6HDRXIcOOgHpVPmHOjTD+DxwcL7TzX0=
  AllowedIPs = ::/0
  " > ~/wireguard/wg3.conf
}

function create() {
  if [ ! -d ~/wireguard ]; then
    mkdir ~/wireguard
  fi

  wg1_conf
  wg3_conf

  ip netns add $NS1
  ip netns add $NS2
  ip netns add $NS3

  ip -n $NS2 link add dev $WG1 type wireguard
  ip -n $NS2 link add dev $WG3 type wireguard

  # establish wg1
  ip -n $NS2 link set dev $WG1 netns $NS1
  ip netns exec $NS1 wg setconf $WG1 ~/wireguard/wg1.conf
  ip -n $NS1 address add fe80::1/64 dev $WG1
  ip -n $NS1 address add fc00::1/64 dev $WG1
  ip -n $NS1 link set dev $WG1 up
  ip -n $NS1 link set lo up
  ip -n $NS1 route add default dev $WG1

  # establish wg3
  ip -n $NS2 link set dev $WG3 netns $NS3
  ip netns exec $NS3 wg setconf $WG3 ~/wireguard/wg3.conf
  ip -n $NS3 address add fe80::2/64 dev $WG3
  ip -n $NS3 address add fc00::2/64 dev $WG3
  ip -n $NS3 link set dev $WG3 up
  ip -n $NS3 link set lo up
  ip -n $NS3 route add default dev $WG3

  ip -n $NS2 link set dev lo up
  ip -n $NS2 route add default dev lo

  # So both options below, the `ip route` and `iptables` options
  # will allow wg interfaces in separate netns to communicate with
  # other. However with the `ip route` option, the peer endpoint field
  # of each wg device will show 127.0.0.1, aka the ip address
  # of 'lo' whereas with the iptables manipulation, the peer endpoints
  # will be IP addresses specified in their respective netns.

#  ip netns exec $NS2 iptables -t nat -A PREROUTING -p udp \
#    --sport 51820 -j DNAT --to-destination 127.0.0.1
#  ip netns exec $NS2 iptables -t nat -A PREROUTING -p udp \
#    --sport 51822 -j DNAT --to-destination 127.0.0.1
#  ip netns exec $NS2 iptables -t nat -A POSTROUTING -p udp \
#    --dport 51820 -j SNAT --to-source 10.2.0.1
#  ip netns exec $NS2 iptables -t nat -A POSTROUTING -p udp \
#    --dport 51822 -j SNAT --to-source 10.0.0.1

  ip -n $NS2 route add fc00::/64 dev lo
  ip -n $NS2 route add fe80::/64 dev lo
}

function clean() {
  ip netns del $NS1
  ip netns del $NS2
  ip netns del $NS3
}

if [ "$1" == "create" ]; then
  create
elif [ "$1" == "clean" ]; then
  clean
else
  echo "Usage: ./wguard.sh [ create | clean ]"
fi
