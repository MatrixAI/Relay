{-
 - Graph definitions and operations
 -
 - ramwan <ray.wan@matrix.ai>
 -
 - https://blogs.ncl.ac.uk/andreymokhov/an-algebra-of-graphs/
 -}

module GraphDefinitions (
  AutomatonGraph (..),
  CompositionGraph (..),
  InstanceGraph (..)
) where

import qualified Algebra.Graph as G
import qualified Algebra.Graph.Labelled as LG
import Control.Monad.State
import System.Random (RandomGen)

import DataDefinitions
import Counter

{-
 - Graph of which automatons need to talk to what. Edges in graph are
 - unidirectional.
 -}
type AutomatonGraph = G.Graph Automaton

{-
 - Similar to AutomatonGraph but contains all the concrete instance information
 - of each automaton instance at the vertices.
 -}
type InstanceGraph = G.Graph [ConcreteInstance]

{-
 - Hypergraph representation of all possible connections of automaton intances
 - to each other. Each edge will have a flowID associated with it so we can pick
 - the best suited communication context.
 -}
type CompositionGraph = LG.Graph FlowID [ConcreteInstance] --placeholder--

{-
 - Graph of which automatons are actually communicating to each other as well as
 - metrics on the communication flow.
 -}
type CommunicationsGraph = LG.Graph FlowID ConcreteInstance --placeholder--



toInstanceGraph :: AutomatonGraph -> InstanceGraph
toInstanceGraph g = foldg G.empty f G.overlay G.connect
                   -- f :: G.Graph Automaton -> G.Graph [ConcreteInstance]
                where f = \ -> -- placeholder

toCompositionGraph :: InstanceGraph -> CompositionGraph

toCommunicationsGraph :: CompositionGraph -> CommunicationsGraph
