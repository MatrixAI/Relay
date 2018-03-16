# Relay Design Prototype

## What are the existing technologies that solve communication between local containers
VLAN

## What are we contributing with Relay
Automated Service Discovery for Containers over multiple Networks
Reproducible Environments for Containers
Content Addressable Containers, Network Locations, and Live Instances

## Service Discovery is a solved problem 
This is already supported by technologies like Envoy. We shouldn't view our
service discovery mechanisms as competing with Envoy, but perhaps work with
already existing service discovery techniques. This can be done using a REST API
specification that is already supported by Envoy service discovery modules.
https://www.envoyproxy.io/docs/envoy/latest/api-v1/cluster_manager/sds.html#config-cluster-manager-sds-api

## For now lets target reproducible environments for containers
Containers already have a content address in the form of a tarball.
We now need to look at content addresses for environments, and see how we can
compose a content addressed environment with a container.

Reproducible environments are already achievable by the nix filesystem.
The proposed focus is on ensuring that automatons are transferrable from machine to machine.
This means content addressing of automatons environment, container, state.
Do we move live automatons around or do we spinup a new automaton?

## What do we want
Automatic deployment of a container and it's environment with no configuration
necessary. We are limited in how far we can go in this respect by 

### Current Goal: 1 Line deployment of a sandbox of simple networked applications
- A small torrenting application
- P2PChat
- Deep Learning frameworks

## Need to pick our integration fights carefully
Where possible, we should avoid reimplementing already existing network
infrastructure in the form of Load balancers, Service Abstraction Layers,
Service Discovery, and multiplexing protocols. Use already existing interfaces,
and support these in the code to reduce work required.

However there are some cases where we will need to 

## What constitutes the minimum required for reproducible environments
- Are we automatically deriving a configuration for containers?
- What's stopping somebody from just chucking a nix file in their project and
    getting the exact same thing that we get?
- If an IP address is put somewhere 

## What is the model for container use along with its environment
- No additional setup required
- Configuration baked in

## Other Ideas
Cache built environments over multiple peers.

