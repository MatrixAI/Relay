#!/bin/bash

SANE=true
SUBNET="/64"
HOSTNET="NURSERY"
HOSTBRIDGE="BRIDGE"
HOSTBRIDGE_ADDR=fc00::1$SUBNET
TYPE=0
N1=1
N2=2
N3=3

#printHelp()
printHelp () {
  echo "Usage: bash $0 [create | clean] [networkconfig] [n1 | n2 | n3]"
  echo "  where [networkconfig] is the directory name and [n1 | n2 | n3"
  echo "  dictates what kind of configuration we want."
  echo "  n1 - NAT rules are instantiated within separate namespaces"
  echo "  n2 - NAT rules are instantiated within the nursery namespace"
  echo "  n3 - no NAT rules"
  echo "  within ./network_configurations"
  echo ""
  echo "DISCLAIMER: this may happen to mess with any other network"
  echo "namespaces you have running. The \"clean\" option will delete"
  echo "all network namespaces."
  echo ""
}

#checkModule(mod_name)
checkModule () {
  if [[ $(grep -e "^$1" | wc -l) -eq 0 ]]; then
    echo "$1 not present."
    SANE=false
  fi
}

createNursery () {
  ip netns add "$HOSTNET"
  ip -n "$HOSTNET" link add "$HOSTBRIDGE" type bridge
  ip -n "$HOSTNET" link set dev lo up
  ip -n "$HOSTNET" link set dev "$HOSTBRIDGE" up
  ip -n "$HOSTNET" address add "$HOSTBRIDGE_ADDR" dev "$HOSTBRIDGE"
}

################################################################################
# SANITY CHECKS

# checkw hether user has permissions to run commands needed
if [[ $EUID -ne 0 ]]; then
  echo "Need to be root. Exiting."
  exit 1 # exit EPERM
fi

if [[ $1 == "clean" ]]; then
  ip -all netns del
  exit 0
elif [[ $1 != "create" ]]; then
  printHelp
  exit 0
fi

# from here on down, we're in the "create" option
if [[ $# -lt 3 ]]; then
  printHelp
  exit 0
fi

# check for NAT configuration options
if [[ "$3" == "n1" ]]; then
  TYPE=$N1
elif [[ "$3" == "n2" ]]; then
  TYPE=$N2
elif [[ "$3" == "n3" ]]; then
  TYPE=$N3
else
  echo "Invalid NAT configuration."
  printHelp
  exit 0
fi

# check if config dir exists
if [[ ! -d ./network_configurations/"$2" ]]; then
  echo "Specified configuration doesn't exist. Exiting."
  exit 2 # exit ENOENT
fi

lsmod | tee >/dev/null \
  >(checkModule "bridge") \
  >(checkModule "veth") \
  >(checkModule "ipv6") \
  >(checkModule "ip6_tables") | paste

if [[ $SANE -ne true ]]; then
  echo "Sanity check failed."
  exit 0
fi

################################################################################
NETS=( $HOSTNET )
IF_PATH=network_configurations/"$2"/tmp/ifs
COUNT=2
ADDR_PREFIX="fc00::"

rm -r network_configurations/"$2"/tmp 2>/dev/null || true
mkdir network_configurations/"$2"/tmp
createNursery

while read -r line;
do
  # a dot at the start of the line indicates a netns we want to add with the
  # name of the netns given by the characters following the dot
  if [[ `echo "$line" | grep "^\." | wc -l` -ne 0 ]]; then
    NETNS=`echo "$line" | sed 's/^\. *//'` 
    LINKBR="${NETNS}_br"
    LINKNET="${NETNS}_net"
    IF_ADDR="${ADDR_PREFIX}${COUNT}${SUBNET}"

    ip netns add "$NETNS"
    ip -n "$HOSTNET" link add "$LINKBR" type veth peer name "$LINKNET"
    ip -n "$HOSTNET" link set "$LINKNET" netns "$NETNS"
    ip -n "$HOSTNET" link set "$LINKBR" master "$HOSTBRIDGE"
    ip -n "$NETNS" link set dev "$LINKNET" up 
    ip -n "$HOSTNET" link set "$LINKBR" up
    ip -n "$NETNS" address add "$IF_ADDR" dev "$LINKNET"

    NETS+=( "$NETNS" )
    COUNT=$(($COUNT + 1))
    echo "$LINKNET@$HOSTNET $IF_ADDR" >> "$IF_PATH"
  fi
done <<< `cat network_configurations/"$2"/conf`

echo "Configuration done. Network namespaces:"
for n in "${NETS[@]}"; do
  echo "$n"
done
