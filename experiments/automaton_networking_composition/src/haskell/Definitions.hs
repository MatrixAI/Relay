{-
 - Functions to operate on these models are found in another file.
 - 
 - ramwan <ray.wan@matrix.ai>
 -}

module Definitions (
) where

import Data.Hashable
import qualified Data.HashMap.Strict as Map
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import qualified Data.IP as IP
import qualified Algebra.Graph as Graph
import qualified Algebra.Graph.Labelled as LGraph

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


{-
  Graph of which automatons need to be able to talk to what. Edges in graph are
  unidirectional.
  With this, we can implement functions to be passed to `foldg` in order to
  transform the automaton graph into a representation of the underlying network.
-}
type CompositionGraph = Graph.Graph Automaton

{-
  Hypergraph representation of the underlying network with nodes being automaton
  instances and the edges of the graph being the flowID of the communication
  context.

  Each hyperedge will have a flowID associated with it. This way we can get
  another target automaton from the set of connected nodes in the hyperedge as
  well as detect whether no instances are available to talk to.
-}
type InstanceGraph = LGraph.Graph FlowID ConcreteInstance

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
 - NOTE: this data type probably won't be needed as flowIDs can be labelled on
 - graph edges.
 -
 - Contains data related to the functionality of the automaton in relation to
 - the Relay network.
 -
data AbstractInstance = AbstractInstance {
                          dependencyFlows :: [FlowID],
                          compositionFlows :: [FlowID],
                                         }
-}

{-
 - Generic Automaton structure containing high level information about the
 - automaton.
 -}
data Automaton = Automaton {
                   name :: Name,
                   instances :: Int
                           } deriving (Show)

{-
 - Data structure containing static information about the composition of the
 - network. This structure should not change unless the network structure needs
 - to be changed.
 -}
data Composition = Composition {
                     automatons :: [Automaton],
                     compositionGraph :: CompositionGraph
                               }


{- ########################################################################## -}

{- https://blogs.ncl.ac.uk/andreymokhov/an-algebra-of-graphs/ -}

-- encodes the relationship between automatons as defined by the user
-- NOTE: edges in Graph.Graph are considered uni-directional although the
-- algebra talked about in the paper and article allow for a generalisation to
-- bidirectional edges
-- // can we use this graph as a midstep between creating instance graphs? can
-- // instance graphs be hypergraphs?? graphs with hyperedges
-- ///// type CompositionGraph = Graph.Graph Automaton

-- having a multilayer network graph allows us to potentially monitor different
-- traffic on the same link - perhaps useful for monitoring interhost
-- communication
-- since this experiment only handles intrahost situations, we can ignore
-- testing multi-layer networks for now

-- graph that encodes kernel level network links. In our intrahost situation, it
-- represents all the network namespaces linked to the flow translator
-- // whats the point of having a graph structure to represent everything being
-- // connected to 1 node? at this point it might as well be a list...
-- type NetworkGraph = Graph.Graph ConcreteInstance

{-
We can represent a "hyperedge" with the `algebraic-graphs` library by doing
foldl overlay empty $ edge <$> [v1] <*> [v2, v3, ... vn]
However if we want to pattern match on this, the resultant "hypergraph" actually
looks like
Overlay (Overlay (Overlay (Overlay Empty (Connect (Vertex 1) (Vertex 2))) ...
rather than
Connect (Vertex 1) (Vertex 2) (Vertex 3) .... (Vertex n)

However using this library we can build a graph of each automaton's connections
and then overlay it all together to create the mapping of the whole automaton
network.

There doesn't seem to be a 'neighbour' function in the library but we can define
our own function like so.
let neighbour' = \x -> \(a, b) -> if x == a then [b]
                                  else if x == b then [a]
                                  else []
neighbour :: a -> Graph a -> [a]
let neighbour n g = concat $ map (neighbour' n) $ edgeList g

-}
