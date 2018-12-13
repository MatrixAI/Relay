#!/bin/bash

# *description*
# by Ramwan <ray@matrix.ai>

SANE=true
HOSTNET="NURSERY"
NETS=( $HOSTNET )
NUMBER_INTERFACES=0
NUMBER_NETS=0
NETCONFIGS="./network_configurations"

# printHelp()
printHelp () {
  echo "Usage: bash ${0} [networkconfig] [create | clean]" 
  echo "  where [networkconfig] is the directory name"
  echo "  within ./network_configurations"
  echo ""
  echo "  DISCLAIMER: this may happen to mess with any other network"
  echo "  namespaces you have running."
  echo "  The 'clean' option will delete all network namespaces."
  echo ""
}

# checkModule(mod_name)
checkModule () {
  if [[ $(grep -e "^$1" | wc -l) -eq 0 ]]; then
    echo "$1 not present."
    SANEfalse
  fi
}

# setNursery()
setNursery () {
  ip netns add ${HOSTNET}
  ip -n ${HOSTNET} link set lo up
}

# parseConfig(ifname, ifnet)
parseConfig () {
  IFNAME=$1
  IFNET=$2

  cat $NETCONFIGS/$CONFIG_NAME/confs/$IFNAME.conf | tee >/dev/null \
    >(ip -n $IFNET address add `sed -n -e 's/#Address = //p'` dev $IFNAME)
    >(
}

# setInterface(config_name, ifname, ifnet, i6addr)
setSapling () {
  CONFIG_NAME=$1
  IFNAME=$2
  IFNET=$3
  I6ADDR=$4

  ip netns add $IFNET
  ip -n $HOSTNET link add $IFNAME type wireguard
  ip -n $HOSTNET link set $IFNAME netns $IFNET

  ip netns exec $IFNET \
      wg setconf ${NETCONFIGS}/${CONFIG_NAME}/confs/${IFNAME}.conf
  ip -n $IFNET link set lo up
  ip -n $IFNET link set $IFNAME up
  
  #ip -n $IFNET address add $I6ADDR dev $IFNAME scope global

  #add iptables and routing rules

  ip -n $IFNET -6 route add default default dev $IFNAME
}

################################################################################
# SANITY CHECKS

# check whether user has permissions to run commands needed
if [[ $EUID -ne 0 ]]; then
  echo "Usage: need to be root."
  exit
fi

if [[ $# -lt 2 ]]; then
  printHelp
  exit
fi

if [[ $2 = "clean" ]]; then
  ip -all netns del
  exit
fi

# check if config dir exists
ls ./network_configurations/${1} > /dev/null
if [[ $? -ne 0 ]]; then
  exit
fi

# we `lsmod` and give it to `tee` where we then pipe the output of `lsmod` to
# fd's as input to the function checkModule and then use `paste` to merge all
# the output
lsmod | tee >/dev/null \
    >(checkModule "ipv6") \
    >(checkModule "udp_tunnel") \
    >(checkModule "ip6_udp_tunnel") \
    >(checkModule "wireguard") \
    >(checkModule "ip_tables") \
    >(checkModule "ip6_tables") | paste

if [[ $SANE -ne true ]]; then
  echo "Sanity check failed."
  exit
fi

################################################################################

NUMBER_NETS=$(ls -l ${NETCONFIGS}/"${1}"/confs/ | wc -l)
echo "Setting up configuration '${1}'" 
echo "Total network namespaces needed: ${NUMBER_NETS}"

setNursery

for f in ./network_configurations/"${1}"/confs/*; do
  [ -e "$f" ] || continue
  IFNAME=$(echo "$f" | sed -e 's/\.conf$//' | sed -e 's/^.*\/confs\///')
  IFNET=(`cat "$f" | sha1sum | tr -d ' -'`)
  NETS+=( "$IFNET" )

  setInterface "$1" "$IFNAME" "$IFNET"
done

for n in "${NETS[@]}"; do
  echo "$n"
done
