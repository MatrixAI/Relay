# Serval: An End-Host Stack for Service-Centric Networking (2012)
<https://www.usenix.org/system/files/conference/nsdi12/nsdi12-final33.pdf>

## Key Ideas
### Address machines by a string depending on what functionality it provides

#### Traditionally, we think of networked machines as

This machine provides some service[^1],  call it 'A', and is located at some particular IP address and port combination. If we don't know this IP address and port combination, we can browse the web to see if we can get this IP address and port combination specifically, or we may have a URL, say "http://api.server.com:1234/A" that represents our service on port number 1234, where the IP address will be resolved by contacting some DNS.

#### Problem 1: 

##### IP and Port number tell us nothing about the functionality of the application

The IP address and port number tells us nothing about the functionality of the service. Because of this, if we do connect to "123.456.7.8:1234", we may find that instead of the service that we want, we get perhaps no response, a 404 error, or a completely different service from the one we wanted. 

In vague and broad terms, the above issue can happen when the service developer makes updates to their application, maintains custom configuration that gets reset to default because they've updated to some new dependent API or they setup their application on a new machine but haven't reproduced the entire environment and configuration of their application. The scenarios in which the above can occur are tricky and buggy, and it can be difficult to provide a simple example that isn't trivial that illustrates the problems that can occur.

Furthermore, neither the service developer nor the developer with an application using the service cares about the particular IP address and port number. It goes in as a configuration detail, and has to be changed on both sides if either the service developer or the application developer makes a configuration change. 

#### Problem 2: 

##### IP and Port Number represents a single machine when a service may be handled by multiple machines

So now, with orchestration tools like Kubernetes that allow us to deploy multiple containers over a cluster of machines, we have a problem: How do we figure out the IP and port number of the target that we actually want to connect to? 

The way this is usually done is to hide the whole service behind a single machine called a load balancer: it serves the dual purpose as the gateway to the service for incoming requests from applications depending on the service, and distributing these requests evenly across the multiple machines that provide the service (replicas of the service). If a machine goes down or you want to upgrade the machines of the cluster to a new version of your service, it is the responsibility of the load balancer to redirect incoming requests to live machines.

This isn't a great way to do it: We run into problem 1 if we change the ip address of the load balancer (we'll probably have to manually change the name record for our load balancer in DNS). And the load balancer is now a bottleneck for incoming requests (for every request to the service, we at least need to get a response from the load balancer with the address of the actual target machine that provides the service we want)

#### Problem *:

And there are other issues as well, which are perhaps worth exploring. From the paper:

> Techniques for handling mobility and migration are either limited to a single layer-2 domain or introduce “triangle routing.” Hosts typically cannot spread a connection over multiple interfaces or paths, and changing interfaces requires the initiation of new connections.

The core issue underlying all the problems here is that addressing a machine using the IP/TCP stack is a bad way to represent a service; it has no semantic relation to the functionality of the service we want, and so should not be a concern of the application and service developers. 

#### What we want: Address Services by Name

So the essential idea of Serval is this: Forget about the IP/TCP stack. Neither the user of the service or the service developer care about what's going on under the hood (as long as the networking is performant). Instead, whenever we want to talk to a service, the only thing that we have to do from the user side is address it using a string representing the service name. For example, instead of:

```python
# Connect to Hello server
# 1234.56.78:1234 is where our Hello service is located (or at least the load balancer of the service is)
sck = connect("1234.56.78:1234")
send(sck, myRequest)
rsp = recvResponse(sck)
```

Addressing services by name could look like this:

```python
hdl = sal.connect("hello")
hdl.write(myRequest)
rsp = recvResponse(hdl)
```

Or it could be like:

```python
# We could make a service abstraction layer that for each service binds an object of the same name under the service abstraction layer, and presents the API of the service immediately at the application level
# May not be a good idea: Object Relational Managers tried to do this for databases and my experience with them hasn't been very pleasant
rsp = sal.hello.recvResponse(myRequest)
```

Where `sal` is a module for the service abtraction layer.

### How is it done: Service Abstraction Layer

The meat of the Serval paper is presenting a technique to do this abstraction of services on top of the existing IP/TCP stack. 

This pretty much comes down to maintaining a lookup table called the _service table_ that maps "service_name" to the relevant identifying information in the IP/TCP stack. 

##### Question: How does the service table do the late binding

#### A (quick?) review of terminology

##### Service

This represents what the application developer is really interested in: the function calls, the API, the thing that you want to get out of the machine independent of its location in the network or the means of communication. 

##### Flow

Referring to an application depending on a service as the host, and the service as the peer. The means of communication may differ between the service and the host

##### Network Address



##### User-Space Control Plane

##### Service-Level Data Plane

##### Service Access

##### Dynamicity

##### Multiplicity



### Another cool feature: Late binding

Claim is:

> ... the SAL can defer binding a service until the packet reaches the part of the network with fine-grain, up-to-date information. This ensures more efficient load balancing and faster failover.

Why?

### Pluggable Service Discovery

Serval makes the claim that they provide 

> a programmable service-level data plane that can adopt diverse service discovery techniques.

How does it do this?

### Portable over Transports and Ability to Combine Flows

Claim:

> The SAL performs signaling between end-points to establish additional flows (over different interfaces or paths) and can migrate them over time. In doing so, the SAL provides a transport-agnostic solution for interface failover, host mobility and virtual-machine migration.

That's a pretty interesting claim. If flow is what I think it means (a connection context), they're saying that they can 

### Extensions

#### Content Addressed Services

The 

### Relevance to Matrix



[^1]: Could be DNS lookup; A HTTP Echo Server that takes a HTTP request and echoes your payload in the HTTP response; A proxy that connects to some peer machine you want on your behalf, forwarding data that you send to the peer and the same in the other direction.]