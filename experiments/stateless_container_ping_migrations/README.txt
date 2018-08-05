GOAL

To demonstrate simple migration of communication flows between network
namespaces pinging each other.

NOT IMPLEMENTED
To demonstrate services running on different network types (eg. ipv4 and ipv6)
communicating with each other.

OVERVIEW

This experiment implements a very simple DNS mechanism in python 3 and a simple
prototype python orchestrator running in its own namespace. Containers
(automatons) are simulated by creating new network namespaces and connecting
them to the orchestrator bridge via veth pairs.

Let S be the container which sends pings.
Let R be the container which responds to pings.
Let O be the orchestrator namespace.

S sends a DNS request to have R's service name resolved. The resolution is
performed by the orchestrator DNS server residing in O and returns an abstract
IP address. The orchestrator edits iptables rules in S's namespace which use NAT
to translate the abstract IP to a concrete IP address and vice versa.

In this current implementation, the iptables updating only is triggered upon a
DNS request (eg. calling `host` or `dig`).
Furthermore, S has to cease sending ICMP ping packets to R for the migration to
occur.
(Possibly due to locks in the kernel)
