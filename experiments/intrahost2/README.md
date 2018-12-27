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
the benchmarking tool.

#### Cases to test

* NAT in each network namespace
* NAT in the nursery namespace
* no NAT
