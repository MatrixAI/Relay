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
import Data.List (mapAccumL)

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
toInstanceGraph g = G.foldg G.empty f G.overlay G.connect g
          -- f :: Automaton -> [ConcreteInstance]
          -- without having `take (instances a)` inside the expression for
          -- addrs, evaluation of `vethns` doesn't appear to stop
          where f a = let
                  caddrs = take (instances a) [minBound :: ConcreteAddr .. ]
                  nf = map (netnsName a) caddrs -- :: [State NameSet NetnsName]
                  nfs = map runState nf -- :: [NameSet -> (NetnsNae, NameSet)]
                  -- :: (NameSet, [NetnsName])
                  (s, netns) = mapAccumL (\s f -> let (n, s') = f s in (s', n)) nEmpty nfs

                  vf = map (vethNames a) netns -- :: [State NameSet VethNames]
                  vfs = map runState vf -- :: [NameSet -> (vethNames, NameSet)]
                  -- :: (NameSet, [VethNames])
                  (s', vethns) = mapAccumL (\s f -> let (ns, s') = f s in (s', ns)) s vfs

                  cInstances = zipWith3 ConcreteInstance caddrs netns vethns
                      in G.vertex cInstances


{-
toCompositionGraph :: InstanceGraph -> CompositionGraph

toCommunicationsGraph :: CompositionGraph -> CommunicationsGraph
-}
