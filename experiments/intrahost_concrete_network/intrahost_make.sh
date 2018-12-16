#!/bin/bash

#=============================================================#
# Description                                                 #
#                                                             #
# by Ramwan <ray@matrix.ai>                                   #
#=============================================================#

SANE=true
HOSTNET="NURSERY"
NETS=( $HOSTNET )
NUMBER_INTERFACES=0
NUMBER_NETS=0
NETCONFIGS="./network_configurations"

# printHelp()
printHelp () {
  echo "Usage: bash $"0" [create | clean] [networkconfig]" 
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

# == Stage 1 ==
#   setSapling(config_base_path, ifname, ifnet, tmp_dir)
#
# - Create the orphan netns for the interface and move it there.
# - Set the config file for the interface
# - Add the ipv6 address to the interface
# - Create a new routing table if specified
setSapling () {
  IFNAME=$2
  IFNET=$3
  TMP_DIR=$4
  CONFIG_PATH="$1"/confs/"$IFNAME".conf
  KEYS_DIR="$1"/keys
  IFPORT=0

  ip netns add "$IFNET"
  ip -n "$HOSTNET" link add "$IFNAME" type wireguard
  ip -n "$HOSTNET" link set "$IFNAME" netns "$IFNET"

  ip netns exec "$IFNET" wg setconf "$IFNAME" "$CONFIG_PATH" 
  
  i6addr=`grep -e '^#Address' "$CONFIG_PATH" | sed -e 's/^.* = //'`
  table=`grep -e '^Table' "$CONFIG_PATH" | sed -e 's/^.* = //'`

  # add ipv6 address to interface and set default route
  ip -n "$IFNET" address add $i6addr dev "$IFNAME"
  if [[ "$table" == "on" ]]; then
    echo "Setting routing table to \"on\" does nothing for now."
  fi

  ip -n "$IFNET" link set lo up
  ip -n "$IFNET" link set "$IFNAME" up
  ip -n "$IFNET" route add default dev "$IFNAME"
  IFPORT=`ip netns exec "$IFNET" wg show "$IFNAME" listen-port`

  python3 config_parser.py\
    "$CONFIG_PATH" "$TMP_DIR" "$KEYS_DIR" "$HOSTNET" "$IFNET" "$IFNAME" "$IFPORT"
  if [[ $? != 0 ]]; then
    echo "Python config parser failed. Handle yourself please."
    exit
  fi
}

################################################################################
# SANITY CHECKS

# check whether user has permissions to run commands needed
if [[ $EUID -ne 0 ]]; then
  echo "Usage: need to be root."
  exit
fi

if [[ $1 == "clean" ]]; then
  ip -all netns del
  exit
elif [[ $1 != "create" ]]; then
  printHelp
  exit
fi

# from here on down, we're in the "create" option
if [[ $# -lt 2 ]]; then
  printHelp
  exit
fi

# check if config dir exists
if [[ ! -d ./network_configurations/"$2" ]]; then
  echo "Specified configuration doesn't exist."
  echo "Exiting."
  exit
fi

type wg >/dev/null 2>&1 || \
  { echo >&2 "\`wg(8)\` isn't installed. Please install."; exit; }
python3 --version >/dev/null 2>&1 || \
  { echo >&2 "\`python3(1)\` isn't installed. Please install."; exit; }

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

NUMBER_NETS=`ls -l "$NETCONFIGS"/"$2"/confs/ | wc -l`
TMP_DIR="$NETCONFIGS"/"$2"/tmp
rm -r "$TMP_DIR" 2>/dev/null || true
mkdir "$TMP_DIR"

echo "Setting up configuration \"$1\"" 
echo "Total network namespaces needed: $NUMBER_NETS"

# Create main network namespace
setNursery

# Create all the child network namespaces. This is a 3 step operation.
# - The first step is to create and move all the interfaces into the correct
#   namespaces and bring them up. 
for f in ./network_configurations/"$2"/confs/*; do
  [ -e "$f" ] || continue
  IFNAME=$(echo "$f" | sed -e 's/\.conf$//; s/^.*\/confs\///')
  IFNET=(`cat "$f" | sha1sum | tr -d ' -'`)
  NETS+=( "$IFNET" )
  BASE_PATH="$NETCONFIGS"/"$2"

  setSapling "$BASE_PATH" "$IFNAME" "$IFNET" "$TMP_DIR"
done

for n in "${NETS[@]}"; do
  echo "$n"
done

