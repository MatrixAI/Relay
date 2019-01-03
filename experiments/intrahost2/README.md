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

All following graphs are made with [plotly](https://plot.ly/create/).

## Purpose

The purpose of this experiment is to determine the effect of NATs on the
throughput of TCP connections and the latency of UDP connections. This
experiment follows on from the previous intrahost experiment as we'll be using
veth pairs as the network instantiation and [Iperf](https://iperf.fr/) again as
the benchmarking tool. However in this experiment we will not be using a bridge.

## Expectation

I expect that the ordering of the iptables rules will have some sort of effect
on the throughput and latency of the tests.

I expect that the initial bitrate of
tests with a lesser amount of rules will be higher than those with a large
amount of rules consistently.

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
`Note the title of the graph is actually the bold text above the graph, not the
text in the graph :)`

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
Another thing to note as shown clearer in that last graph was that appending the
actual nat rule to the bottom of the list appears to give slightly better
throughput.

Following are the UDP results. The UDP results follow a similar pattern to the
TCP results, showing that having NAT rules instantiated in the nursery namespace
gives a higher throughput with no nat entries giving the highest throughput.

__UDP throughput comparison between No Nat and appended to list__
![9](https://web.archive.org/web/20190103054243im_/https://raw.githubusercontent.com/MatrixAI/Relay/master/experiments/intrahost2/graphs/udp_throughput_append_comparison.png)
__UDP throughput comparison between No Nat and middle of list__
![10](https://web.archive.org/web/20190103054303im_/https://raw.githubusercontent.com/MatrixAI/Relay/master/experiments/intrahost2/graphs/udp_throughput_middle_comparison.png)
__UDP throughput comparison between No Nat and top of list__
![11](https://web.archive.org/web/20190103054326im_/https://raw.githubusercontent.com/MatrixAI/Relay/master/experiments/intrahost2/graphs/udp_throughput_top_comparison.png)

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

__UDP throughput comparison between No Nat and various positions in the list__
![8](https://web.archive.org/web/20190103054224im_/https://raw.githubusercontent.com/MatrixAI/Relay/master/experiments/intrahost2/graphs/udp_nonat_vs_topmiddleappend_nursery_nat1k_comparison.png)

```
Key for above graph
1. No nat
2. 1000 nat entries in nursery at top of list
3. 1000 nat entries in nursery at middle of list
4. 1000 nat entries in nursery at bottom of list
```

It seems that having the rule at the very bottom of the list gives a higher
throughput for the test although the difference isn't really significant,
especially once the machine starts talking with an external network where the
network link will be the limiting factor rather than the network address
translation.
However of course practically it would also be impossible to have each active
NAT rule at the bottom.

Another expectation I had was that the initial bitrate would be significantly
different for different amounts of NAT entries but this was not noticeable.
__Comparison of TCP bitrate on interval 0.00-1.00 for Append Nat10 and Nat1000__
![9](https://web.archive.org/web/20190103065330im_/https://raw.githubusercontent.com/MatrixAI/Relay/master/experiments/intrahost2/graphs/append_nat10_nat1k_tcp_01_comparison.png)

```
Key for above graph
1. Append Nat10 TCP on interval 0.00-1.00
2. Append Nat1000 TCP on interval 0.00-1.00

Note: there was a huge outlier for the Nat10 entry in this graph (bitrate of
27 Gbits/sec) which I have cut off in the image).
```

__Comparison of UDP bitrate on interval 0.00-1.00 for Append Nat10 and Nat1000__
![10](https://web.archive.org/web/20190103070248im_/https://raw.githubusercontent.com/MatrixAI/Relay/master/experiments/intrahost2/graphs/append_nat10_nat1k_udp_01_comparison.png)
```
Key for above graph
1. Append Nat 10 UDP on interval 0.00-1.00
2. Append Nat1000 UDP on interval 0.00-1.00
```

Overall compared to the previous experiment regarding Wireguard performance, it
can be seen that iptables with veth pairs provides a much better choice for
intrahost communication between automatons. The number of NAT rules in iptables
appears to not affect the throughput and latency which means having the NAT
rules in the nursery should theoretically continue to give higher performance
than having each automaton's namespace contain the rules for the automaton
(because the nursery would have all the rules for the physical host whereas each
automaton would only have rules specific to itself).

I haven't tested how the system behaves if we were to have many connections
traversing the nursery although I'm pretty sure this will have a negative impact
on the throughput and latency but at that point I believe that iptables won't be
the bottleneck - rather it should be external network or the automatons won't be
outputting that many packets.

The results of this experiment show that iptables is able to be used but should
probably be investigated further.

An example of the abstract flow transformations is shown below. All commands
need to be run with root privilege.
```
ip -all netns del
ip netns add A
ip netns add B
ip netns add C
ip -n B link add vA type veth peer name vAB
ip -n B link add vC type veth peer name vCB
ip -n B link set dev vA netns A
ip -n B link set dev vC netns C
ip -n A link set dev lo up
ip -n A link set dev vA up
ip -n B link set dev lo up
ip -n B link set dev vAB up
ip -n B link set dev vCB up
ip -n C link set dev lo up
ip -n C link set dev vC up

ip -n A address add fc00::1/64 dev vA
ip -n C address add fd00::1/64 dev vC
ip -n B address add fc00::2/64 dev vAB
ip -n B address add fd00::2/64 dev vCB

ip netns exec B sysctl net.ipv6.conf.all.forwarding=1

ip -n A -6 route add default dev vA via fc00::2
ip -n C -6 route add default dev vC via fd00::2

ip netns exec B ip6tables -t nat -A PREROUTING -d fe00::1 -j DNAT \
  --to-destination fd00::1
ip netns exec B ip6tables -t nat -A POSTROUTING -s fc00::1 -j SNAT \
  --to-source fe00::1
```
From this point, you can run `ip netns exec A ping fe00::1` and `ip netns exec C
tcpdump` in separate terminals and watch the traffic.

* As an aside on the above setup, iptables rules are able to actually change but
  the change won't be actually implemented until the conntrack entry is removed.
