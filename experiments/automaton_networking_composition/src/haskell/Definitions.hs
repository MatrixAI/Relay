{-
 - Functions to operate on these models are found in another file.
 - 
 - ramwan <ray.wan@matrix.ai>
 -}

module Definitions (
  Name,
  FlowID,
  ConcreteAddress,
  Mapping,
  FlowTable,
  ConcreteInstance,
  AbstractInstance,
  Automaton,
  Composition
) where

import Data.Hashable
import qualified Data.HashMap.Strict as Map
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import qualified Data.IP as IP

instance Hashable IP.IPv6

-- deterministically generated automaton name
type Name = String
-- flowID for a communication context
-- flowIDs are of the IPv6 range fd00::/7 -> unique-local range
type FlowID = IP.IPv6
-- concrete address of an automaton
-- concrete addresses are of the IPv6 range fc00::7 -> unique-local range
type ConcreteAddress = IP.IPv6
-- Automaton A will talk on FlowID in order to communicate with Automaton B
type Mapping = (FlowID, Automaton) -- (FlowID, Automaton B)
-- Hashmap in order to facilitate lookup by the name translation system for
-- translating packets flowing across the hub
type FlowTable = Map.HashMap FlowID Automaton

-- Simple generic graph structure represented as G = {V, E}
-- This probably will change later.
type Vertex = Int
type Sender = Vertex
type Receiver = Vertex
type Node a = (Vertex, a)
-- edges in our graph are unidirectional so we make the distinction between
-- sender and receiver
type Edge = (Sender, Receiver)
type Graph a = ([Node a], [Edge])
-- type MultiEdge = (Sender, Receiver, SenderL, ReceiverL)
-- type MultilayerGraph a = ([Graph a], [MultiEdge])

-- encodes the relationship between automatons as defined by the user
-- // can we use this graph as a midstep between creating instance graphs? can
-- // instance graphs be hypergraphs?? graphs with hyperedges
type CompositionGraph = Graph Automaton

-- having a multilayer network graph allows us to potentially monitor different
-- traffic on the same link - perhaps useful for monitoring interhost
-- communication
-- since this experiment only handles intrahost situations, we can ignore
-- testing multi-layer networks for now

-- graph that encodes kernel level network links. In our intrahost situation, it
-- represents all the network namespaces linked to the flow translator
-- // whats the point of having a graph structure to represent everything being
-- // connected to 1 node? at this point it might as well be a list...
-- type NetworkGraph = Graph ConcreteInstance

{-
 - Contains data related to the functionality of the automaton in relation to
 - the kernel.
 -}
data ConcreteInstance = ConcreteInstance {
                          netns :: Name,
                          veths :: (Name, Name),
                          gateway :: ConcreteAddress
                                         }

{-
 - Contains data related to the functionality of the automaton in relation to
 - the Relay network.
 -}
data AbstractInstance = AbstractInstance {
                          dependencyFlows :: [FlowID],
                          compositionFlows :: [FlowID],
                          concreteInstance :: ConcreteInstance
                                         }

{-
 - Generic Automaton structure containing high level information about the
 - automaton.
 -}
data Automaton = Automaton {
                   name :: Name,
                   instances :: Int
                           }

{-
 - Data structure containing static information about the composition of the
 - network. This structure should not change unless the network structure needs
 - to be changed.
 -}
data Composition = Composition {
                     automatons :: [Automaton],
                     compositionGraph :: CompositionGraph
                               }
