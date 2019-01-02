#!/usr/bin/bash
# ramwan <ray.wan@matrix.ai>

NUM_RULES=0
LOCATION="client"

usage () {
  echo "Run with root privilege."
  echo "bash make_config.sh -h"
  echo "  display this help message."
  echo "bash make_config.sh [-n num] -l [client | nursery]"
  echo "  -n num sets the number of iptables rules in the client namespace."
  echo ""
  echo "NOTE: the NAT entry in iptables to reach the server from the client is"
  echo "added at the end of all other bogus entries."
}

while getopts ":l:n:h:" o; do
  case "${o}" in
    h)
      usage
      exit
      ;;
    l)
      if [[ "$OPTARG" -eq "client" ]] || [[ "$OPTARG" -eq "nursery" ]]; then
        LOCATION=$OPTARG
      else
        echo "Bad location."
        exit 1
      fi
      ;;
    n)
      if [[ ! "$OPTARG" =~ ^[0-9]+$ ]]; then
        echo "Bad number."
        exit 1
      fi
      NUM_RULES=$OPTARG
      ;;
    *)
      ;;
  esac
done

if [[ $EUID -ne 0 ]]; then
  echo "Need to be root."
  exit 1
fi

ip netns add server
ip netns add client
ip netns add nursery

ip -n server link set dev lo up
ip -n nursery link set dev lo up
ip -n client link set dev lo up

ip -n nursery link add slnk type veth peer name slnkbridge
ip -n nursery link add clnk type veth peer name clnkbridge

ip -n nursery link set dev slnk netns server
ip -n nursery link set dev clnk netns client

ip -n server link set dev slnk up
ip -n nursery link set slnkbridge up
ip -n nursery link set clnkbridge up
ip -n client link set clnk up

ip -n server address add fd00::1/64 dev slnk
ip -n client address add fc00::1/64 dev clnk
ip -n nursery address add fd00::2/64 dev slnkbridge
ip -n nursery address add fc00::2/64 dev clnkbridge

ip -n server -6 route add default dev slnk via fd00::2
ip -n client -6 route add default dev clnk via fc00::2

ip netns exec nursery sysctl net.ipv6.conf.all.forwarding=1 > /dev/null

echo "Created network namespaces:"
ip netns
echo ""

echo "Server endpoint at fd00::1/64"
echo "Client endpoint at fc00::1/64"

BASE_FLOW="fe00::"
SUBNET_RANGE="/64"
for i in `seq 2 $((NUM_RULES+1))`; do
  if [[ "$LOCATION" == "client" ]]; then
    eval $(\
      ip netns exec client ip6tables -t nat -A OUTPUT -d "${BASE_FLOW}${i}" \
      -j DNAT --to-destination fc00::10)
  elif [[ "$LOCATION" == "nursery" ]]; then
    eval $(\
      ip netns exec nursery ip6tables -t nat -A PREROUTING -d "${BASE_FLOW}${i}" \
      -j DNAT --to-destination fc00::10)
  fi
done
if [[ $NUM_RULES -gt 0 ]]; then
  echo "Created $NUM_RULES ip6tables entries in client netns."
  echo "The server can now also be found at fe00::1."

  if [[ "$LOCATION" == "client" ]]; then
    eval $(\
      ip netns exec client ip6tables -t nat -A OUTPUT -d "${BASE_FLOW}1" \
      -j DNAT --to-destination fd00::1)
  elif [[ "$LOCATION" == "nursery" ]]; then
    eval $(\
      ip netns exec nursery ip6tables -t nat -A PREROUTING -d "${BASE_FLOW}1" \
      -j DNAT --to-destination fd00::1)
  fi
fi
