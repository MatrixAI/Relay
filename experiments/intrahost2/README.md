# Intrahost Experiment 2

The first intrahost experiment (found at ../intrahost_concrete_network)
investigated the possibilities of using [Wireguard](https://www.wireguard.com/)
for connecting namespaces in the Relay network. From using
[Iperf](https://iperf.fr/), we found that Wireguard was an order of magnitude
slower than veth pairs on a Linux bridge and had an order of magnitude more
latency as well even within the single host. Another issue discovered was that
Wireguard endpoints act as only a server or a client and not both meaning
servers cannot initiate connections with clients. Despite Wireguard being a poor
choice for intrahost communications, Wireguard is still an option for interhost
communications across cloud vendors.

## Purpose

The purpose of this experiment is to determine the effect of NATs on the
throughput of TCP connections and the latency of UDP connections. This
experiment follows on from the previous intrahost experiment as we'll be using
veth pairs as the network instantiation and [Iperf](https://iperf.fr/) again as
the benchmarking tool. However in this experiment we will not be using a bridge.

## Expectation

I expect that the ordering of the iptables rules will have some sort of effect
on the throughput and latency of the tests.

I don't expect there to be a difference in terms of having the NAT rules in the
nursery or the client namespace but the lack of NAT rules in the control should
mean that throughput is higher and latency is lower.

## Experiment Design

#### Cases to test

* NAT in each network namespace
* NAT in the nursery namespace
* no NAT

Note that we cannot use NAT without Netfilter conntrack.

#### Test setup

The tests will be done on Linux 4.14.66 x86_64 disconnected to all other
networks and will show the effect of various NAT setups across a single link.
My machine is a quadcore i7-4600U @ 2.1GHz.

Dependencies:
* ip (iproute2-ss180129 or higher although any modern version should work fine)
* ip6tables v1.6.2 (although any modern version should work fine)
* iperf 3.5 (although any modern version should work fine)
* bash 4.4.12 (although any remotely modern version should work fine)

__make_config.sh__ is the script which sets up the network namespaces in the
format
```
Server --- Nursery --- Client
```
It will also instantiate ip6tables rules in either the Nursery or the Client
namespace and at a given position in the list. The veth endpoint in the client
is set to fc00::1/64 and the veth endpoint in the server is at fd00::1/64 with
the 2 endpoints in the Nursery namespace set to fc00::2/64 and fd00::2/64
respectively.
An example run of __make_config.sh__ could be
```
sudo bash make_config.sh -n 10 -l nursery -p top
```

Following, the iperf server needs to be instantiated in the Server namespace.
This is done by
```
sudo ip netns exec server iperf -s
```

Then in a separate terminal run
```
sudo bash run_client.sh fe00::1 false [log_file_directory]
```
where fe00::1 is the server endpoint via NAT, false indicates we don't want a
UDP test and the log_file_directory is where you want to save (1..15).out.

Following each run of iperf, __run_client.sh__ will clear the conntrack table by
running `conntrack -F` in each of the network namespaces.

## Results

https://web.archive.org/web/20190103053925/https://github.com/MatrixAI/Relay/blob/master/experiments/intrahost2/graphs/ping_latency_natappend.png
https://web.archive.org/web/20190103053953/https://github.com/MatrixAI/Relay/blob/master/experiments/intrahost2/graphs/ping_latency_natmiddle.png
https://web.archive.org/web/20190103054019/https://github.com/MatrixAI/Relay/blob/master/experiments/intrahost2/graphs/ping_latency_nattop.png
https://web.archive.org/web/20190103054051/https://github.com/MatrixAI/Relay/blob/master/experiments/intrahost2/graphs/tcp_nonat_vs_topmiddleappend_nursery_nat1k_comparison.png
https://web.archive.org/web/20190103054113/https://github.com/MatrixAI/Relay/blob/master/experiments/intrahost2/graphs/tcp_throughput_append_comparison.png
https://web.archive.org/web/20190103054141/https://github.com/MatrixAI/Relay/blob/master/experiments/intrahost2/graphs/tcp_throughput_middle_comparison.png
https://web.archive.org/web/20190103054203/https://github.com/MatrixAI/Relay/blob/master/experiments/intrahost2/graphs/tcp_throughput_top_comparison.png
https://web.archive.org/web/20190103054224/https://github.com/MatrixAI/Relay/blob/master/experiments/intrahost2/graphs/udp_nonat_vs_topmiddleappend_nursery_nat1k_comparison.png
https://web.archive.org/web/20190103054243/https://github.com/MatrixAI/Relay/blob/master/experiments/intrahost2/graphs/udp_throughput_append_comparison.png
https://web.archive.org/web/20190103054303/https://github.com/MatrixAI/Relay/blob/master/experiments/intrahost2/graphs/udp_throughput_middle_comparison.png
https://web.archive.org/web/20190103054326/https://github.com/MatrixAI/Relay/blob/master/experiments/intrahost2/graphs/udp_throughput_top_comparison.png
