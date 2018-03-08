# Envoy Architecture

<https://www.envoyproxy.io/docs/envoy/latest/intro/what_is_envoy>

<https://blog.envoyproxy.io/introduction-to-modern-network-load-balancing-and-proxying-a57f6ff80236>

## Where to start?

Recall the OSI model:

![OSI Model][]

Envoy claims that it is a proxy operating at L7, the application layer. Intuitively, we consider this to mean it is a router/proxy for services (a process or application that has been containerised). 

Envoy also claims that it is a sidecar process, which intuitively means that 

## Key Ideas

### Difference between an L4 and L7 Proxy/Load Balancer

#### What's going on in L4

![L4 Load Balancer][]

For every request that the client sends at Layer 4, a new TCP connection will be opened between the source tuple of (IP, port) and dest tuple. The important thing here is that __the load balancer terminates the incoming connection and opens a new connection from the backend to the client__

The blog post on L4 vs. L7 provides the following example:

> - Two [gRPC](https://grpc.io/)/[HTTP2](https://en.wikipedia.org/wiki/HTTP/2) clients want to talk to a backend so they connect through an L4 load balancer.
> - The L4 load balancer makes a single outgoing TCP connection for each incoming TCP connection, resulting in two incoming and two outgoing connections.
> - However, client A sends 1 request per minute (RPM) over its connection, while client B sends 50 requests per second (RPS) over its connection.



What do we gain by operating at L7?



### Transparent Network to Applications

> The network should be transparent to applications. When network and application problems do occur it should be easy to determine the source of the problem.

How is network transparency for applications achieved in the Envoy model?







[OSI Model]: http://1.bp.blogspot.com/-yPHtWF43D8c/TY-PxT-KqNI/AAAAAAAAAAo/1OaBHHrsEKE/s1600/osi-model-7-layers.png
[L4 Load Balancer]: https://cdn-images-1.medium.com/max/800/1*1PjTpM3hLnm3iEAd4-_AaQ.png

