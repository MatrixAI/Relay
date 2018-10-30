{-
 - Simple resource counter.
 -
 - ramwan <ray.wan@matrix.ai>
 -}

module Counter (
  NameSet, nEmpty,
  allocate,
  netnsName, vethNames
) where

import Control.Monad.State (State, state, runState)
import Data.Hashable
import Data.HashSet

import DataDefinitions

-- HashSet of all the taken kernel level resource names as far as the
-- Orchestrator knows so we don't double up
type NameSet = HashSet Name


netnsNameLength = 20
vethNameLength = 15

-- create an infinite list of hashed values
infHash :: String -> [String]
infHash s = drop 1 $ iterate showHash s

showHash :: Hashable a => a -> String
showHash = show . hash

showSaltedHash :: Hashable a => Int -> a -> String
showSaltedHash n a = show $ hashWithSalt n a


-- hashset operations for convenience
nEmpty :: NameSet
nEmpty = empty

nMember :: Name -> NameSet -> Bool
nMember = member

nInsert :: Name -> NameSet -> NameSet
nInsert = insert

nDelete :: Name -> NameSet -> NameSet
nDelete = delete


-- no tracking of deallocated resources
-- failure is NOT checked...
allocate :: Enum a => State a a
allocate = state $ \n -> (n, succ n)


-- Takes an infinite list of names and takes the first one that hasn't already
-- been allocated.
-- 
-- eg. \n ns -> allocName (iterate showHash n) ns
allocName :: [Name] -> NameSet -> Name
allocName (n:ns) s
          | nMember n s = allocName ns s
          | otherwise   = n



-- The network namespace name can be calculated by 
-- netnsName = take 20 $ hash $ (name Automaton) ++ (show concreteAddr)
-- This way the name is somewhat deterministic.
netnsName :: Automaton -> ConcreteAddr -> State NameSet NetnsName
netnsName a ca = state $
                   \s -> let
                           n = name a ++ show ca
                           n' = take netnsNameLength $
                                  allocName (infHash n) s
                         in (n', nInsert n' s)


vethName' :: Name -> State NameSet VethName
vethName' n = state $
                \s -> let
                        n1 = take vethNameLength $
                               allocName (infHash n) s
                      in (n1, nInsert n1 s)

-- Veth endpoint names can be calculated by
--   let n = netnsName ++ automatonName
--   vethNames = (hash n, hash hash n)
-- Like netns name generation, this is somewhat deterministic.
vethNames :: Automaton -> NetnsName -> State NameSet VethNames
vethNames a netn = vethName' (name a ++ netn)
                   >>= \n1 -> vethName' n1
                   >>= \n2 -> return (n1, n2)
