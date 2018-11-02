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
import Data.List (mapAccumR, mapAccumL)
import Data.Maybe (fromJust)
import qualified Data.Set as DS (toList)
import qualified Data.HashMap.Lazy as HM

import DataDefinitions
import Counter

{-
 - Graph of which automatons need to talk to what. Edges in graph are
 - unidirectional. We can create 'bidirectional' edges by doing
 - connect x y + connect y x
 -}
type AutomatonGraph = G.Graph Automaton

{-
 - Similar to AutomatonGraph but contains all the concrete instance information
 - of each automaton instance at the vertices.
 -
 - ISSUE: How do we represent undirected edges? With the current implementation,
 - we'd need to have `overlay (connect [c1..] [c2..]) (connect [c2..] [c1..])`
 - which would also mean that we're generating different ConcreteInstance
 - templates for what is actually the same Automaton.
 -}
type InstanceGraph = G.Graph [ConcreteInstance]

{-
 - Hypergraph representation of all possible connections of automaton intances
 - to each other. Each edge will have a flowID associated with it so we can pick
 - the best suited communication context.
 -}
type CompositionGraph = LG.Graph FlowID ConcreteInstance --placeholder--

{-
 - Graph of which automatons are actually communicating to each other as well as
 - metrics on the communication flow.
 -}
type CommunicationsGraph = LG.Graph FlowID ConcreteInstance --placeholder--


-- TODO: fix ISSUE stated in the type declation of InstanceGraph
--       persist STATE across multiple calls of f
toInstanceGraph :: AutomatonGraph -> InstanceGraph
toInstanceGraph g = let
    v = DS.toList $ G.vertexSet g
    e = DS.toList $ G.edgeSet g
-- use mapAccumL instead of mapAccumR to preserve the correspondence of
-- Automaton to [ConcreteInstance] in their respective lists. If we used
-- mapAccumR we'd just need to `reverse` one of the lists
    (gs', a) = mapAccumL automatonToConcreteInstance (nEmpty, minBound) v
    m = createHashMap v a
    f v = G.vertex $ fromJust $ HM.lookup v m
      in G.foldg G.empty f G.overlay G.connect g


-- State monad?
-- not sure how lazy eval will go and whether we need `const` or `seq` anywhere
automatonToConcreteInstance :: GeneratorState -> Automaton -> (GeneratorState, [ConcreteInstance])
automatonToConcreteInstance gs@(ns, cas) a = let
              caddrs = take (instances a) [cas .. ]
              cas' = succ $ last caddrs
              nf = map (netnsName a) caddrs
              nfs = map runState nf
              (ns', netns) = mapAccumR (\s f -> let (n, s') = f s in (s', n))
                             ns nfs

              vf = map (vethNames a) netns
              vfs = map runState vf
              (ns'', vethns) = mapAccumR (\s f -> let (ns, s') = f s in (s', ns))
                               ns' vfs

              cInstances = zipWith3 ConcreteInstance caddrs netns vethns
                in ((ns'', cas'), cInstances)

createHashMap :: [Automaton] -> [[ConcreteInstance]] -> HM.HashMap Automaton [ConcreteInstance]
createHashMap a ci = foldr f HM.empty aZipCI
              where f e hm = (uncurry $ HM.insert) e hm
                    aZipCI = zip a ci

-----------


--toCompositionGraph :: InstanceGraph -> CompositionGraph
--toCompositionGraph g = G.foldg LG.empty f LG.overlay LG.connect g


--toCommunicationsGraph :: CompositionGraph -> CommunicationsGraph
