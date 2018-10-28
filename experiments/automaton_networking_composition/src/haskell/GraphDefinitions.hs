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


{-
 - Create templates of automaton instances in a graph representation.
 - foldg :: b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> Graph a -> b
 - foldg e v o c = go
 -    where
 -      go Empty         = e
 -      go (Vertex  x  ) = v x
 -      go (Overlay x y) = o (go x) (go y)
 -      go (Connect x y) = c (go x) (go y)
 -
 - eg. foldg G.empty
 -           (f :: G.Graph Automaton -> G.Graph [Concrete Instance])
 -           G.overlay
 -           G.connect
 -           (g :: G.Graph Automaton)
 -}
--aToCI :: Automaton -> [ConcreteInstance]
--aToCI (Automaton _ x )
--          | x <= 0    = []
--          | otherwise = 


makeConcreteInstance :: 
makeConcreteInstance = let
                         (nn, g1) = runState netnsName g
                         (vn, g2) = runState vethNames g1


{-
genConcreteInstances :: (Eq n, Num n, RandomGen g) =>
                          n -> State g [ConcreteInstance]
genConcreteInstances n
          | n<=0 = []
          | otherwise = let 
                          (ci, g') = runState genCI
                        in [ci:genConcreteInstances (n-1) g']
-}
