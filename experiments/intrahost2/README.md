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

#### Cases to test

* NAT in each network namespace
* NAT in the nursery namespace
* no NAT

Note that we cannot use NAT without Netfilter conntrack.

## Test setup

The tests will be done on Linux 4.14.66 x86_64 disconnected to all other
networks and will show the effect of various NAT setups across a single link.

Dependencies:
* ip (iproute2-ss180129 or higher although any modern version should work fine)
* ip6tables v1.6.2 (although any modern version should work fine)
* iperf 3.5 (although any modern version should work fine)

https://web.archive.org/web/20190102094856im_/https://raw.githubusercontent.com/MatrixAI/Relay/master/experiments/intrahost2/ping_latency.png

https://web.archive.org/web/20190102094936if_/https://raw.githubusercontent.com/MatrixAI/Relay/master/experiments/intrahost2/tcp_throughput_comparison.png

https://web.archive.org/web/20190102094958if_/https://raw.githubusercontent.com/MatrixAI/Relay/master/experiments/intrahost2/udp_throughput.png
