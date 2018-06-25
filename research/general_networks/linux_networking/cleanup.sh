# cleanup file for sample_bridge_veth.sh

sudo ip link delete $VETH_0n
sudo ip link delete $BRIDGE_NAME
sudo ip netns delete $NS_NAME1
sudo ip netns delete $NS_NAME2
