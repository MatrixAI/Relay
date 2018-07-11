## Design Document

#### Contents
```
  1. Overall Design
    1.1 Overview of Relay Network
```

#### 1. Overall Design
```
- Automatons need to be deployable within and outside a Matrix network
  without any changes to the code by the programmer.
- Abstraction
- containerised microservice
- service abstraction
  - Relay sidecar service
    - flow and abstract network addresses
    - flow table
- service discovery and routing
- flow migration
```
##### 1.1 Terms used
```
Automaton
  what is an automaton

Matrix
  what is Matrix

Relay
  what is Relay
```
##### 1.2 Overview of Relay network
```
- allow automatons to communicate with each other
- implementation of high level service abstractions
- balance load across automatons
- route requests efficiently
- allow the creation or removal of automatons
- allow, as best as possible, communication flow to persist through
  ephemeral services
- Automatons need to be able to run inside and outside a Matrix network with no
  changes
```
##### 1.3 Service abstraction
```
- network namespaces
- communication context and flow
- application sockets and flow identifiers

A service is composed of one or more microservices. In Matrix, no
assumptions are made as to the physical location of these microservices and as
such they may reside in completely different networks from each other.

Some examples of services include a webapp consisting of a webserver and a
datastore. The microservices in this situation would be the webserver and the
datastore.

As such automatons that rely on other automatons to function (as in the
webserver and datastore example) have a dependcy. Matrix is concerned with
network dependencies rather than build or runtime dependencies. Having network
dependencies means the services are more scalable as automatons depend on other
automatons rather than being tied to a specific physical machine providing that
service.

There are three ways which automatons can be composed with each other; object,
functional and union.

Object composition of automatons is the same as composition of objects in an OOP
language where object A 'has a' object B in it.

Functional composition is to compose simple functions together to form a more
complex one where the output of one function is passed as input to the next.
With Automatons, the basic idea is that the output of one automaton is fed as
the input to another. However the way protocols work may mean that automatons
may actually communicate in a bi-directional byte stream. Another way to look at
functional composition of automatons is for the composed automatons to work as
one automaton where the order of composition is important. An interesting thing
to point out is that functional composition means that when an automaton gives
input to another automaton, it may actually receive the result back from a
different automaton.

Union composition can be used when multiple automatons have compatible protocol
specifications. The easiest way to demonstrate this is if two automatons are
both HTTP web servers, each serving separate routes. Note, separate routes need
to be served otherwise a request arriving at this composed virtual automaton
will not know where to be directed. This union composition is similar to the
functionality of an API gateway which acts as a service which directs requests
for varying clients to the appropriate backend servers.


When automatons are deployed, they will have a service name assigned to it. This
service name can represent any number of automatons. An example of the purpose
of this should make this clearer.

  Automaton A needs to talk to Automaton B.
  There is only one A but two Bs.
  A sends a request to B's service name which could get resolved to B1 or B2,
  but A doesn't care as long as the request gets satisfied.
  A has no knowledge of the number of Bs and doesn't care.

By communicating on the abstract service name, developers of automatons can
have very loose coupling between services and are able to easily scale
deployments.

A requirement of automatons is that they need to be able to run within a Matrix
network or outside of it as a standalone container without any changes by
developers. As such, service names will need to be URLs conforming to RFC3986.
Furthermore, if the service is run outside a Matrix network and references
domain names that need to be resolved by standard DNS infrastructure, developers
will need to make sure that these names actually resolve. If domain names are
used as service names within the Matrix network however, it doesn't matter
whether the names resolve or not as the automatons will not be communicating
with the outside world.


An automaton within a service will 

```
##### 1.4 Service discovery and routing
```
- TODO
```
##### 1.5 Service and flow migration
```
- TODO
```

#### References
http://microservices.io/patterns/apigateway.html
