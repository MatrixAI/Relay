ADDRESS=$1
ISUDP=$2
DIR=$3

if [[ "$1" == "-h" ]] || [[ "$1" == "--help" ]] ; then
  echo "Usage: bash run_client.sh ADDRESS ISUDP DIR"
  echo "  where ADDRESS is the server address, ISUDP is true or false"
  echo "  and DIR is the dir to save the logs to."
  exit
fi

if [[ $EUID -ne 0 ]]; then
  echo "Need to be root."
  exit 1
fi

for i in `seq 1 15`; do
  echo "###################################################"
  echo "$i"
  echo "###################################################"

  if [[ "$ISUDP" == "true" ]]; then
    ip netns exec client iperf -c "$ADDRESS" -b 0 -t 10 -u \
      | tee "$DIR"/$i.out
  else
    ip netns exec client iperf -c "$ADDRESS" -b 0 -t 10 \
      | tee "$DIR"/$i.out
  fi
  sleep 2
  ip netns exec client conntrack -F
  ip netns exec nursery conntrack -F
  ip netns exec server conntrack -F
done 
