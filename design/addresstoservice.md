For any given protocol, and a library that supports that protocol given an
address supported by that protocol, we need to map the peer addresses to some
location that uses our content address network system to talk to peers.

Perhaps for our automaton layer we should figure out how to redirect the flows
of the application to the correct content addressed machine flow.

Okay:

So let's start with a fork of P2PChat, or start with a simple application
What if we use the serval stack to do the remapping
We can't modify the dependent libraries of the protocol.

So we are doing a simple occurrence check for an application that does the
environment remapping.

Okay, let's construct the simplest example possible.

- Create a content hash of the Hello World server
- Use the content hash in a http GET request


