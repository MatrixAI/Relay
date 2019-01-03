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

#### Terms
When I mention 'nursery', it's just a name I've given to an intermediary network
namespace which acts as a router between 2 other network namespaces.

Top, middle and append are positions that refer to the position of an element in
a list if the list is traversed from top to bottom in sequential order.

NAT - network address translation

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
running `conntrack -F` in each of the network namespaces. __run_client.sh__
tests each setup with both NAT entries in the nursery and client side with
__15 runs per test__. This can be modified if you wish in __run_client.sh__. I
picked 15 because I didn't want to spend forever waiting for all the tests to
finish. :)

## Results

Firstly let's talk about ping latency. In all the tests, the control (no nat)
had the least jitter which was interesting but also had the lowest or roughly
equally lowest mean latency. This was pretty much expected.

__Latency comparison of No Nat with rule appended to list__
![1](https://web.archive.org/web/20190103053925im_/https://raw.githubusercontent.com/MatrixAI/Relay/master/experiments/intrahost2/graphs/ping_latency_natappend.png)
__Latency comparison of No Nat with rule in the middle of the list__
![2](https://web.archive.org/web/20190103053953im_/https://raw.githubusercontent.com/MatrixAI/Relay/master/experiments/intrahost2/graphs/ping_latency_natmiddle.png)
__Latency comparison of No Nat with rule prepended to the list__
![3](https://web.archive.org/web/20190103054019im_/https://raw.githubusercontent.com/MatrixAI/Relay/master/experiments/intrahost2/graphs/ping_latency_nattop.png)

```
Key for above graphs
1. No nat
2. 10 nat entries in client side
3. 10 nat entries in nursery
4. 100 nat entries in client side
5. 100 nat entries in nursery
6. 1000 nat entries in client side
7. 1000 nat entries in nursery
```

Following are the TCP results.
These results show that the situation without NAT performs a decent amount
better than those with NAT, which was expected. However we're testing in an
extremely ideal environment where there is minimal other network traffic. What
was surprising though was that having the NAT rules in the nursery allowed a
higher throughput consistently.

__TCP throughput comparison of No Nat with rule appended to the list__
![5](https://web.archive.org/web/20190103054113im_/https://raw.githubusercontent.com/MatrixAI/Relay/master/experiments/intrahost2/graphs/tcp_throughput_append_comparison.png)
__TCP throughput comparison of No Nat with rule in the middle of the list__
![6](https://web.archive.org/web/20190103054141im_/https://raw.githubusercontent.com/MatrixAI/Relay/master/experiments/intrahost2/graphs/tcp_throughput_middle_comparison.png)
__TCP throughput comparison of No Nat with rule prepended to the list__
![7](https://web.archive.org/web/20190103054203im_/https://raw.githubusercontent.com/MatrixAI/Relay/master/experiments/intrahost2/graphs/tcp_throughput_top_comparison.png)

```
Key for above graphs
1. No nat
2. 10 nat entries in client side
3. 10 nat entries in nursery
4. 100 nat entries in client side
5. 100 nat entries in nursery
6. 1000 nat entries in client side
7. 1000 nat entries in nursery
```
__TCP throughput comparison between No Nat and various positions in the list__
![4](https://web.archive.org/web/20190103054051im_/https://raw.githubusercontent.com/MatrixAI/Relay/master/experiments/intrahost2/graphs/tcp_nonat_vs_topmiddleappend_nursery_nat1k_comparison.png)

```
Key for above graph
1. No nat
2. 1000 nat entries in nursery at top of list
3. 1000 nat entries in nursery at middle of list
4. 1000 nat entries in nursery at bottom of list
```

![8](https://web.archive.org/web/20190103054224im_/https://raw.githubusercontent.com/MatrixAI/Relay/master/experiments/intrahost2/graphs/udp_nonat_vs_topmiddleappend_nursery_nat1k_comparison.png)
![9](https://web.archive.org/web/20190103054243im_/https://raw.githubusercontent.com/MatrixAI/Relay/master/experiments/intrahost2/graphs/udp_throughput_append_comparison.png)
![10](https://web.archive.org/web/20190103054303im_/https://raw.githubusercontent.com/MatrixAI/Relay/master/experiments/intrahost2/graphs/udp_throughput_middle_comparison.png)
![11](https://web.archive.org/web/20190103054326im_/https://raw.githubusercontent.com/MatrixAI/Relay/master/experiments/intrahost2/graphs/udp_throughput_top_comparison.png)
