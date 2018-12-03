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
  InstanceGraph (..),
  toInstanceGraph, toCompositionGraph
) where

import Algebra.Graph as G
import Algebra.Graph.Labelled as LG
import Algebra.Graph.Label
import Control.Monad.State
import System.Random (RandomGen)
import Data.List (mapAccumR, mapAccumL)
import Data.Maybe (fromJust)
import qualified Data.Set as DS (toList)
import qualified Data.HashMap.Lazy as HM
import Data.Hashable (Hashable)

import DataDefinitions
import Counter

type Edge a = (a, a)

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
type InstanceGraph = G.Graph ConcreteInstances

{-
 - Hypergraph representation of all possible connections of automaton intances
 - to each other. Each edge will have a flowID associated with it so we can pick
 - the best suited communication context.
 -}
type CompositionGraph = LG.Graph FlowID ConcreteInstance

{-
 - Graph of which automatons are actually communicating to each other as well as
 - metrics on the communication flow.
 -}
type CommunicationsGraph = LG.Graph FlowID ConcreteInstance --placeholder--

--------------------------------------------------------------------------------

toInstanceGraph :: AutomatonGraph -> InstanceGraph
toInstanceGraph g = let
    v = DS.toList $ G.vertexSet g

-- use mapAccumL instead of mapAccumR to preserve the correspondence of
-- Automaton to ConcreteInstances in their respective lists. If we used
-- mapAccumR we'd just need to `reverse` one of the lists
    (_, a) = mapAccumL automatonToConcreteInstance (nEmpty, minBound) v
    m = kvToMap v a
    f = \i -> G.vertex $ fromJust $ HM.lookup i m
      in G.foldg G.empty f G.overlay G.connect g

-- State monad?
-- state monad.
-- flip :: (a->b->c)->b->a->c
-- concreteAutomaton :: Automaton -> State GeneratorState ConcreteInstances
-- concreteAutomaton = flip automatonToConcreteInstance
-- not sure how lazy eval will go and whether we need `const` or `seq` anywhere
automatonToConcreteInstance ::
        GeneratorState -> Automaton -> (GeneratorState, ConcreteInstances)
automatonToConcreteInstance gs@(ns, cas) a = let
              -- State monad? something nicer and more elegant than this plz...
              caddrs = take (instances a) [cas .. ]
              cas' = succ $ last caddrs

              nf = map (netnsName a) caddrs
              nfs = map runState nf
              (ns', netns) = mapAccumR accumF ns nfs

              vf = map (vethNames a) netns
              vfs = map runState vf
              (ns'', vethns) = mapAccumR accumF ns' vfs

              cInstances = zipWith3 ConcreteInstance caddrs netns vethns
                in ((ns'' `seq` ns'', cas' `seq` cas'), cInstances)

-- state/accumulator -> runState -> (accumulator, result)
accumF :: s -> (s -> (a, s)) -> (s, a)
accumF s f = let (n, s') = f s in (s', n)

-- rename to something more meaningful
kvToMap :: (Hashable k, Eq k) => [k] -> [v] -> HM.HashMap k v
kvToMap ks vs = foldr f HM.empty kZipV
              where f e m = (uncurry $ HM.insert) e m
                    kZipV = zip ks vs

--------------------------------------------------------------------------------

toCompositionGraph :: InstanceGraph -> CompositionGraph
toCompositionGraph g = let
        es = DS.toList $ G.edgeSet g -- :: [Edge ConcreteInstances]
        fs = [minBound :: FlowID ..]

{-
        we need the first element of each element in `es` to be paired up with a
        flowID and then have (\(x, f)
-}

      map fst es

      -- esMAPfs :: (s, [(ConcreteInstan
          -- x :: ConcreteInstances
          -- where x is taken as the first element of each element in `es`
        esMAPfs = mapAccumL (\s x -> let
                                (a, s') = runState allocate s
                              in (s', (x, a))
                        ) (minBound :: FlowID) $ map fst es

        es' = concat $ map (\(x, y) -> permute x y) es -- :: [Edge ConcreteInstance]
        fsANDes = zip es' fs
          in labelledEdges fsANDes


permute :: [a] -> [b] -> [(a, b)]
permute as bs = [(x, y) | x<-as, y<-bs]

-- Labelled graph version of G.edges
labelledEdges :: (Semilattice e, Ord e) => [(Edge a, e)] -> LG.Graph e a
labelledEdges           []       = LG.empty
labelledEdges (((a1, a2), e):xs) = ((LG.vertex a1) -<e>- (LG.vertex a2))
                                        `LG.overlay` labelledEdges xs

--------------------------------------------------------------------------------

--toCommunicationsGraph :: CompositionGraph -> CommunicationsGraph
--toCommunicationsGraph g =




--------------------------------------------------------------------------------
-- Helper functions

aConnect :: Automaton -> Automaton -> AutomatonGraph
aConnect x y = G.connect (G.vertex x) (G.vertex y)
