#!/bin/bash

#=============================================================#
# Description                                                 #
#                                                             #
# by Ramwan <ray@matrix.ai>                                   #
#=============================================================#

SANE=true
HOSTNET="NURSERY"
HOST_HASH=`echo "$HOSTNET" | sha1sum | sed -e 's/ -$//'`
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
  ip netns add "$HOSTNET"
  ip -n "$HOSTNET" link set lo up
}

# == Stage 1 ==
#   setSapling(config_base_path, tmp_dir, ifname, ifnet)
#
# - Create the orphan netns for the interface and move it there.
# - Set the config file for the interface
# - Add the ipv6 address to the interface
# - Create a new routing table if specified
setSapling () {
  BASE_PATH=$1
  TMP_DIR=$2
  IFNAME=$3
  IFNET=$4
  CONFIG_PATH="$BASE_PATH"/confs/"$IFNAME".conf
  KEYS_DIR="$BASE_PATH"/keys
  IFPORT=0

  ip netns add "$IFNET"
  ip -n "$HOSTNET" link add "$IFNAME" type wireguard
  ip -n "$HOSTNET" link set "$IFNAME" netns "$IFNET"
  ip netns exec "$IFNET" wg setconf "$IFNAME" "$CONFIG_PATH" 
  
  table=`grep -e '^Table' "$CONFIG_PATH" | sed -e 's/^.* = //'`
  if [[ "$table" == "on" ]]; then
    echo "Setting routing table to \"on\" does nothing for now."
  fi

  ip -n "$IFNET" link set lo up
  ip -n "$IFNET" link set "$IFNAME" up
  IFPORT=`ip netns exec "$IFNET" wg show "$IFNAME" listen-port`
  IFADDR=`python3 address_generator.py "real" "$HOST_HASH" $IFPORT "$IFNET"`

  line=`sed -n -e '/^ *#[^ ]*$/ p' "$CONFIG_PATH" \
        | tail -n +2 | sed -e 's/^#//' | tr '[:upper:]' '[:lower:]'`

  while read -r peer; do
    FLOW=$(python3 address_generator.py "flow" "$HOST_HASH" \
              $IFPORT "`echo "$peer" | sha1sum | sed -e 's/ -$//'`")
    echo "$IFNAME $peer $FLOW" >> "$TMP_DIR"/CONNS
  done <<< "$line"
  PUBKEY=`cat "$KEYS_DIR"/"$IFNAME".pub | sed -e 's!/!\\\/!g'`
  echo "$IFNAME $IFADDR $IFPORT $IFNET $PUBKEY" >> "$TMP_DIR"/IFS

  ip -n "$IFNET" address add "$IFADDR" dev "$IFNAME"
  ip -n "$IFNET" -6 route add default dev "$IFNAME"
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
BASE_PATH="$NETCONFIGS"/"$2"
NUMBER_NETS=`ls -l "$BASE_PATH"/confs/ | wc -l`
TMP_DIR="$BASE_PATH"/tmp
rm -r "$TMP_DIR" 2>/dev/null || true
mkdir "$TMP_DIR"

echo "Setting up configuration \"$1\"" 
echo "Total network namespaces needed: $NUMBER_NETS"

# Create main network namespace
setNursery

# ============== Stage 1 ==============
# Create all the child network namespaces. This is a 3 step operation.
# - The first step is to create and move all the interfaces into the correct
#   namespaces and bring them up. 
for f in ./network_configurations/"$2"/confs/*; do
  [ -e "$f" ] || continue
  IFNAME=`echo "$f" | sed -e 's/\.conf$//; s/^.*\/confs\///' | \
          tr '[:upper:]' '[:lower:]'`
  IFNET=(`cat "$f" | sha1sum | tr -d ' -'`)
  NETS+=( "$IFNET" )

  setSapling "$BASE_PATH" "$TMP_DIR" "$IFNAME" "$IFNET"
done

echo "== Stage 1 ==  done"
echo "Created network namespaces:"
for n in "${NETS[@]}"; do
  echo "$n"
done

# ============== Stage 2 ==============
# Set the routing and iptables rules necessary for namespaces to communicate.

python3 conn_uniq.py "$TMP_DIR"/CONNS

# each line of the CONNS.uniq file is of the format "wg1 wg2 flow"
# Build a file of iptables rules pre transform.
awk -f runscripts/iptables.awk "$TMP_DIR"/CONNS.uniq \
  | sed -e "s/\$HOSTNET/$HOSTNET/" \
  > "$TMP_DIR"/iptables.pre

# Build a file of wg endpoint rules pre transform.
awk -f runscripts/endpoints.awk "$TMP_DIR"/CONNS.uniq > "$TMP_DIR"/ifs_wg.pre

# Build a sedscript used to substitute interface names with their relevant
# addresses.
echo "{" > "$TMP_DIR"/ifs_iptables.sed
awk -f runscripts/interface.awk "$TMP_DIR"/IFS \
  >> "$TMP_DIR"/ifs_iptables.sed
echo "}" >> "$TMP_DIR"/ifs_iptables.sed

# Build a sedscript used to substitute interface names with relevant values.
echo "{" > "$TMP_DIR"/ifs_wg.sed
awk -f runscripts/endpoints_sub.awk "$TMP_DIR"/IFS \
  >> "$TMP_DIR"/ifs_wg.sed
echo "}" >> "$TMP_DIR"/ifs_wg.sed

# Run said sedscript and transform iptables rules
sed -f "$TMP_DIR"/ifs_iptables.sed "$TMP_DIR"/iptables.pre \
  | sort | uniq > "$TMP_DIR"/iptables.post

# Run said sedscript and transform wg endpoint rules
sed -f "$TMP_DIR"/ifs_wg.sed "$TMP_DIR"/ifs_wg.pre > "$TMP_DIR"/ifs_wg.post

# ============== Stage 3 ==============
# At this point we have 2 files that contain all the commands that we need to
# run in order to set up the network - iptables.post and ifs_wg.post, both
# residing in $TMP_DIR

while read -r line; do
  eval "$line"
done <<< `cat "$TMP_DIR"/iptables.post`

while read -r line; do
  eval "$line"
done <<< `cat "$TMP_DIR"/ifs_wg.post`
