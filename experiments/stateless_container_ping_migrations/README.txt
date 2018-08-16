################################################################################

________________________________________________________________________________
NOTE some of the things I talk about in the body of this writeup are addressed 
at the very bottom - namely the reason why the experiment wasn't quite working.
--------------------------------------------------------------------------------


PRIMARY GOAL

To simulate automatons being able to communicate with each other across an
abstract address and have the receiving end of the communication migrate
transparently to a different network address.

Simulation of communication is done by having different network namespaces ping
each other.

SECONDARY GOALS

1. To explore the feasibility of using iptables for network address translation.

2. To bring forward and discuss any implementation issues and find new
   directions to take.

3. To test the ability for machines residing on different networks to
   communicate with each other (eg. IPv4 host communicating with IPv6 host).

4. To simulate the initiator of communication flow migrate transparently to the
   receiver.

5. Examine the possibility of using 'multiaddresses'.

################################################################################

OVERVIEW

This experiment aims to demonstrate automatons communicating with each other by
simulating them with separate linux network namespaces sending ICMP ping
requests to each other via a bridge.

    +----------------------------------------------------------------+
    |                                                                |
    | +-----+     +-----+     +-----+   +-----+                      |
    | |a.com|     |b.com|     |a.com|   |c.com|                      |
    | +--+--+     +--+--+     +--+--+   +--+--+                      |
    |    |           |           |         |                         |
    |    |           |           |         |                         |
    |    +-----------+-----+-----+---------+         Host            |
    |                      |                                         |
    |                      |                                         |
    |            +---------+---------+                               |
    |            |orchestrator bridge|                               |
    |            +-------------------+                               |
    |                                                                |
    +----------------------------------------------------------------+
                        Experiment network setup

The above diagram shows how the experiment sets up the network namespaces.
a.com, b.com and c.com are service names which will be explained later but they
each reside in separate namespaces. The orchestrator is a linux bridge which
resides in a separate namespace as well. Each namespace is connected to the
bridge namespace via veth pair.

To allow developers to deploy containers within a Matrix network and outside of
one without having to do extra work, service names potentially will be regular
domain names. When inside a Matrix network, we don't care whether the developers
own the names or not as the requests to resolve these names never reach the
wider internet.

This leads to the implementation of name resolving. When 'a.com' wants to
communicate to 'b.com' (in this case, ICMP ping), it sends a request to resolve
the name 'b.com'. The orchestrator namespace has a DNS server listening and
accepts this request for 'b.com' and looks up whether an instance of 'b.com'
exists. If no instance of 'b.com' exists, the DNS server will indicate that the
name cannot be resolved. 

However if 'b.com' exists, the DNS server won't be returning the network address
of an instance of 'b.com', but it will return an abstract network address
indicating a communication flow between the initiator and receiver. The instance
of 'a.com' that wants to talk to an instance of 'b.com' will now send requests
to the abstract network address.

To the program, by all intents and purposes the instance which it communicates
with will reside on this abstract address. However because this abstract address
is only a mapping, either side of the communication flow can migrate
transparently.


RUNNING THE EXPERIMENT

The most basic sample of the experiment can be run as follows. Be aware that the
way the experiment deletes network namespaces is 'ip -all netns del' so if you
have important namespaces (eg. running docker containers) then either feel free
to edit the code or shut down those namespaces beforehand.

You also need to have linux bridges enabled on your device as well as python3
installed.

`sudo python3 orchestrator_main.py`
`h`
`n a.com`
`n b.com`
`a a.com /ipv4/10.0.1.1`
`a b.com /ipv4/10.0.2.1`

At this point you will have 3 network namespaces - the bridge and 2 random
strings. In a different terminal window, run `ip netns` to check what those
random names are.

Then run `ip netns exec _name_of_one_of_the_namespaces_ ip a` where
_name_of_one_of_the_namespaces_ is one of the namespaces shown when running `ip
netns`. By checking the ip address of the veth endpoint, you can see whether
you're in the namespace of 'a.com' or 'b.com'.

`ip netns exec _name_of_same_namespace_you_checked_above_ host
_other_service_name_`
Where _name_of_same_namespace_you_checked_above_ is the name of the same
namespace you checked above and _other_service_name_ is 'a.com' if the namespace
you 'exec'ed into was an instance of 'b.com' and vice versa if the instance was
'a.com'.

You should see a response along the lines of
'a.com has address 10.0.254.0'
';; Warning: Message parser reports malformed message packet.'

Ignore the warning. The important part is 10.0.254.0 - the abstract network
address or 'flow id'. 

`ip netns exec _name_of_same_namespace_you_checked_above_ ping _returned_ip_`
where _returned_ip_ is the ip you received from the DNS request (eg.
10.0.254.0). You should now be pinging the other service instance.

Ctrl+C to cancel the ping program and go back to the window where the python
program is running.

`d _instance_you_pinged_`
`a _instance_you_pinged_ /ipv4/10.0.1.2`

Where _instance_you_pinged_ is the instance you resolved with `host`. The old
instance is now deleted and a new one created on a new ip address. Swap to the
other terminal window and run

`ip netns exec _name_of_same_namespace_you_checked_above host
_other_service_name`

You should have the same abstract ip (flow id) returned. Run the ping command
from above again with the same flow id and you should be seeing the namespaces
sending ping requests to each other as before the service migration.

################################################################################

IMPLEMENTATION

This section will go further into detail the implementation of the experiment
and hopefully you will be able to point out some things I may have missed or
could have done better. Note that this experiment is a first attempt at this
solution format so the design and structure can definitely be improved.

'orchestrator_main.py' is the main file for the program. It starts a thread to
listen for user input as well as launches a thread for the DNS server.

'multiaddress.py' is a file for handling conversions between data formats. This
is a little module I wrote myself, albeit poorly as it scales badly and gets
very confusing as I will discuss in the discussion section.

'ipt_handler.py' has functions that handle the saving and reloading of the
iptables settings before and after the experiment is run. The function of
interest here though is 'create_nat_mapping(flow_id, source_ip, dest_ip)'.

'mini_dns.py' is the module which launches a thread to listen for DNS requests.
The function '_get_flow_id(serv_name, sender_addr)' is of interest as it calls
into code that allocates connection tracking and flow ids.

'networking_handler.py' handles the creation of the initial bridge, any network
namespaces, veth pairs, bringing up interfaces, moving interfaces to the correct
namespace and enslaving veth endpoints to the bridge. Objects of importance are
at the top of the file, namely the bridge ipv4 multiaddress, the subnet mask and
'class Namespace'.

'service_handler.py' is the interface between the UI thread and code which
manipulates datastructures and networking details. Calls into
'service_flow_structures.py'.

'service_flow_structures.py' is probably the largest and most important file in
this experiment. It has all the data structures used to keep track of
connections and flows and the mappings of concrete network addresses to abstract
flows. 'get_flow_id(serv_name, v4=None, v6=None)' is the function which got
called in 'mini_dns.py' to check whether a flow already exists and create new
flows as necessary. This function is probably the entrypoint to look at when
trying to figure out how flow tracking is done. Admittedly, the implementation
is rather messy and not very well designed as will be discussed later.

-

This experiment is written in python but can be thought of as a program which
gives a friendlier interface into running bash commands, mainly `ip(8)` and
`iptables(8)`. By using existing command line interfaces to kernel modules, I
was able to save a lot of time as I didn't have to dig through kernel code and
documentation to figure out what I needed to do.

However it doesn't allow a lot of control over what happens and when. For
example, iptables commands are run one at a time which could result in
potentially semi-configured firewall states which definitely cannot happen in
production environments but for the purposes of this experiment, it is ok as it
is a proof of concept.

One note though is that I intended to save iptables configurations to a file,
parse and edit it and then call 'iptables-restore(8)' to get around the issue of
semi-configured firewall states but this ended up not being implemented. However
the idea is still there (early functionality is implemented and idea is
alright).

################################################################################

ACHIEVED GOALS

Secondary goals 2,4,5


PARTIALLY ACHIEVED GOALS

Primary goal
Secondary goal 1,3

################################################################################

DISCUSSION

I would say I was partially successful in the primary goal of getting one
namespace to ping another over an abstract address and have the receiving end
migrate transparently. Automaton simulations were able to communicate with each
other via abstract addresses and migration was able to be demonstrated which is
exciting but the migration wasn't very transparent.

For example in the instructions to run the experiment, you had to exit `ping(8)`
before closing a service instance. Upon creating a new service instance, you
also had to re-resolve the service name.

If `ping(8)` was left running, and the old receiver instance was deleted and a
new one created, `ping(8)` would still appear to hang. Upon exiting ping and
resending a DNS request, there would be significant delay in the name resolution
but after a period of time, the resolution would procede as normal.

This leads me to assume that there are locks in kernel space associated with
running programs and xtables. Perhaps they even share the same lock because we
aren't able to edit iptables rules while a program is actively using it.

As a result, further examination into xtables code and looking into BPF and
kernel modules is required to see whether the functionality we need is possible
with current linux features. If not, perhaps we need to consider whether a
kernel module will have the desired effects and whether it is actually feasible
to do what this experiment is attempting to prototype - it's still possible that
our ideas of the level of transparency are a bit far-fetched.

-

The migration of the initiator of the communication flow is conceptually much
the same as migrating the receiver. However implementation details potentially
will differ but seeing as the receiver is able to migrate, migrating the
initiator should (theoretically) be possible even though this experiment does
not demonstrate it.

-

The use of multiaddresses was also examined in this experiment. As the
experiment grew in size, I found it more and more difficult to keep track of
exactly what format of multiaddress was passed around, whether it be a string in
the format '/ipv4/10.0.0.1' or 'packed' format of a tuple such as ('ipv4',
'10.0.0.1'). This may have been due to python's lack of strict typing or my poor
naming schemes for function arguments or a combination of both.

The aim for this multiaddress though was to allow one abstraction to represent
multiple network address formats (and later on, any protocol and address). This
would segregate logic to check what kind of network the host resides on into one
place rather than having all sorts of checks everywhere. It also allows the
simple addition of new networks with different formats transparently to other
modules.

In the experiment however, I found that since I only ended up dealing with IPv4,
the multiaddress was too much. Service information was stored with
multiaddresses but flow information and connection tracking was stored with
string network addresses.

Despite having troubles with multiaddresses in this experiment, I think that in
later experiments, multiaddresses should be tested again to see whether it helps
simplify communication flows. This experiment probably shouldn't be taken as an
indicator that multiaddresses aren't useful or shouldn't be used because the
situation just didn't require it.

On that note, Matrix overlay networks are intended to run purely in IPv6 -
automatons communicate with each other via IPv6. However interhost communication
may still have to occur across different kinds of networks - orchestrators may
reside on any kind of network and assumptions shouldn't be made on Matrix's
part.

-

One of the requirements for Matrix networks is to have things that run within it
also be able to run outside of it which is why service names (automaton names)
are 'a.com' and 'b.com'. This allowed us to actually resolve names via DNS.
However names are intended to be passed into the orchestrator after being
deterministically generated from the automaton specification (architect
language).

What this means for name resolution is that if we cannot use or are simply not
given a name resolvable via regular DNS methods, we cannot use DNS. We can take
DNS's idea though.

-

Another point to note is that this experiment only prototypes STATELESS
migration. ICMP ping has sequence numbers and this was not migrated with
instances.

However in discussions about state migration, it's been brought up that the
Matrix network potentially doesn't have to worry about the details of the
migration of state as such details are often very specific to what is being
migrated. Let the developers who know what they're doing do that!

This that Relay potentially need to try and do though are the redirection of
communication streams after a service has been migrated. An example of this is
if communication between two services is in HTTP, it doesn't make sense to send
half a HTTP request to one receiver and the other half to a new instance of the
receiver, unless the socket states have also been migrated.

In the same HTTP example though, if we were to send multiple HTTP requests out
to retrieve content for a web page, it would be ok for whole HTTP requests to
hit different service instances as the requests would still make sense and be
able to be responded to.

################################################################################

NOTES ON FURTHER DIRECTION

- examine xtables kernel code
- BPF filters
- state communication between orchestrators on different physical hosts
- security requirements within a Matrix network
- quality of service constraints
- clarify orchestrator design in order to find more focus with emphasis on
  multi-host functionality





More notes after reading more into kernel code
----------------------------------------------

Xtables code for the function `xt_replace_table()` can be found at
net/netfilter/x_tables.c. I'm thinking that this is the function which upgrades
an xtable configuration which is what an iptables update eventually will call.
Inside we can see a reference to a seqlock.

With this knowledge, I ran the experiment again and tried to swap out the target
service while `ping` was running. The ping program proceeded to fail
(specifically, it gave "Destination Host Unreachable") messages.

I disregarded this and tried to resolve a service anyway to set the flowID in
the orchestrator to the new IP of the new service instance. The first `host`
call failed, perhaps it was just due to my implementation (or python being
dodgy) as sometimes DNS requests do fail.

A second attempt at `host a.com` however returned the same flowID. The original
`ping` program continued to print out "Destination Host Unreachable" messages
but I was able to run a second ping program which was able to ping the flowID
successfully.

Running `tcpdump` while having the old ping program running (the one with host
unreachable) and the new ping which is able to use the same flowID. I've added a
log of the tcpdump output for those interested to go through as well in this
directory.

My suspicion is that this has something to do with conntrack as indeed there is
no lock in the kernel preventing iptables from changing underneath a program.

Upon repeating the experiment but running `conntrack` to check the connections
in the constant namespace, we can see that the old connection still exists
(conntrack_migration_output file in this directory). 

Running the experiment one more time but this time editing the conntrack table
as well (`sudo ip netns exec *namespace* conntrack -D conntrack`) proved to be
enough as the original ping program which would've started showing host
unreachable messages now proceeds as if nothing had changed (although icmp_seq
numbers were off) as shown below.

"""
ray@examples: sudo ip netns exec MTQyNQ== ping 10.0.254.0
PING 10.0.254.0 (10.0.254.0) 56(84) bytes of data.
64 bytes from 10.0.254.0: icmp_seq=1 ttl=64 time=0.064 ms
64 bytes from 10.0.254.0: icmp_seq=2 ttl=64 time=0.041 ms
64 bytes from 10.0.254.0: icmp_seq=3 ttl=64 time=0.040 ms
64 bytes from 10.0.254.0: icmp_seq=4 ttl=64 time=0.047 ms
64 bytes from 10.0.254.0: icmp_seq=39 ttl=64 time=0.073 ms
64 bytes from 10.0.254.0: icmp_seq=40 ttl=64 time=0.048 ms
64 bytes from 10.0.254.0: icmp_seq=41 ttl=64 time=0.042 ms
64 bytes from 10.0.254.0: icmp_seq=42 ttl=64 time=0.051 ms
64 bytes from 10.0.254.0: icmp_seq=43 ttl=64 time=0.049 ms
64 bytes from 10.0.254.0: icmp_seq=44 ttl=64 time=0.050 ms
"""


