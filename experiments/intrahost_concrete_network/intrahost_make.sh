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

  peers=`sed -n -e '/^ *#[^ ]*$/ p' "$CONFIG_PATH" \
        | tail -n +2 | sed -e 's/^#//' | tr '[:upper:]' '[:lower:]'`
  for peer in "${peers[@]}"; do
    FLOW=$(python3 address_generator.py "flow" "$HOST_HASH" \
              $IFPORT "`echo "$peer" | sha1sum | sed -e 's/ -$//'`")
    echo "$IFNAME $peer $FLOW" >> "$TMP_DIR"/CONNS
  done
  echo "$IFNAME=[$IFADDR]:$IFPORT/$IFNET" >> "$TMP_DIR"/IFS
}

#  python3 config_parser.py\
#    "$CONFIG_PATH" "$KEYS_DIR" "$IFNET" "$IFNAME" "$IFADDR" "$IFPORT" \
#    >> "$TMP_DIR"/"$IFNAME"
#  # If the config parser fails then we exit and clean up after ourselves.
#  if [[ $? != 0 ]]; then
#    echo "Python config parser failed on $IFNAME."
#    exit && `bash intrahost_make.sh clean`
#  fi

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

python3 conn_uniq.py "$TMP_DIR"/CONNS

# ip -n $HOSTNET route add $DEST dev lo

# ip -n $HOSTNET iptables -6 -t nat -A PREROUTING -p udp -d $FLOW -s $SRC  -j DNAT --to-destination $DEST
# ip -n $HOSTNET iptables -6 -t nat -A POSTROUTING -p udp -s $SRC -d $DEST -j SNAT --to-source $FLOW
# ip -n $HOSTNET iptables -6 -t nat -A PREROUTING -p udp -d $FLOW -s $DEST -j DNAT --to-destination $SRC
# ip -n $HOSTNET iptables -6 -t nat -A POSTROUTING -p udp -s $DEST -d $FLOW -j SNAT --to-source $FLOW

sed -e "s/\([^ ]*\) \([^ ]*\) \([^ ]*\)$/ip -n \"$HOSTNET\" \
  iptables -6 -t nat -A PREROUTING -p udp -d \3 -j DNAT \
  --to-destination \2/" "$TMP_DIR"/CONNS.uniq

for n in "${NETS[@]}"; do
  echo "$n"
done

