{-
 - https://blogs.ncl.ac.uk/andreymokhov/an-algebra-of-graphs/
 -}

module GraphDefinitions (
) where

import qualified Algebra.Graph as G
import qualified Algebra.Graph.Labelled as LG

import DataDefinitions

{-
 - Graph of which automatons need to talk to what. Edges in graph are
 - unidirectional.
 -}
type AutomatonGraph = G.Graph Automaton

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
type InstanceGraph = G.Graph ConcreteInstance
