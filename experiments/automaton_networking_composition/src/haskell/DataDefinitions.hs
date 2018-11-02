{-
 - Functions to operate on these models are found in another file.
 - 
 - ramwan <ray.wan@matrix.ai>
 -}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- what does this do?
{-# LANGUAGE DeriveGeneric              #-}

module DataDefinitions (
  Name, AutomatonName, NetnsName, VethName, VethNames,
  IFChar (..),
  FlowID, ConcreteAddr,
  flowID, concreteAddr,
  Mapping,
  FlowTable,
  ConcreteInstance (..), Automaton (..),
  checkFlowID, checkConcAddr
) where

import Data.Hashable
import qualified Data.HashMap.Strict as Map
import Data.IP (IPv6)
import System.Random (Random, RandomGen, random, randomR, next)
import Data.Maybe (fromJust)
import GHC.Generics (Generic)

import HelperFunctions (readIP)

-- deterministically generated automaton name
type Name = String
type AutomatonName = Name
type NetnsName = Name
type VethName = Name
type VethNames = (VethName, VethName)



ifChars = ['0'..'9']++['A'..'Z']++['a'..'z']
table = map IFChar ifChars
zipTable = zip table [0..]

newtype IFChar = IFChar Char deriving (Eq, Ord, Show, Read)
instance Bounded IFChar where
  minBound = IFChar '0'
  maxBound = IFChar 'z'

instance Enum IFChar where
  fromEnum = fromJust . flip lookup zipTable
  toEnum = fromJust . flip lookup (map (\(x, y) -> (y, x)) zipTable)

instance Random IFChar where
  randomR (l, h) g 
      | l>h = randomR (h, l) g
      |otherwise = (toEnum $ (a + v `mod` k), g')
                 where (v, g') = next g
                       a = fromEnum l
                       b = fromEnum h
                       k = b - a + 1
  random g = randomR (minBound, maxBound) g



-- flowID for a communication context
-- flowIDs are of the IPv6 range fd00::/7 -> unique-local range
-- TODO: newtype FlowID and ConcreteAddr so we can create unique Bounded
-- instances
newtype FlowID = FlowID IPv6
                 deriving (Enum, Eq, Ord, Read, Show)
instance Bounded FlowID where
  minBound = fID "fd00::"
  maxBound = fID "fdff:ffff:ffff:ffff:ffff:ffff:ffff:ffff"


-- concrete address of an automaton
-- concrete addresses are of the IPv6 range fc00::7 -> unique-local range
newtype ConcreteAddr = ConcreteAddr IPv6
                       deriving (Enum, Eq, Ord, Read, Show)
instance Bounded ConcreteAddr where
  minBound = cAddr "fc00::"
  maxBound = cAddr "fcff:ffff:ffff:ffff:ffff:ffff:ffff:ffff"



-- Automaton A will talk on FlowID in order to communicate with Automaton B
type Mapping = (FlowID, Automaton) -- (FlowID, Automaton B)


-- Hashmap in order to facilitate lookup by the name translation system for
-- translating packets flowing across the hub
type FlowTable = Map.HashMap FlowID Automaton


{-
 - Contains data related to the functionality of the automaton in relation to
 - the kernel.
 -}
data ConcreteInstance = ConcreteInstance {
                          gateway :: ConcreteAddr,
                          -- 256 bytes including trailing NULL
                          netns :: NetnsName,
                          -- IF namesize is 16 bytes with trailing NULL
                          veths :: (VethName, VethName)
                                         } deriving (Show)

{-
 - Generic Automaton structure containing high level information about the
 - automaton.
 -
 - TODO: perform checking on `instances` to make sure it's >= 0
 -}
data Automaton = Automaton {
                   name :: AutomatonName,
                   instances :: Int
                           } deriving (Show, Eq, Ord, Generic)
instance Hashable Automaton

-- Helper functions
fID :: String -> FlowID
fID = FlowID . readIP
cAddr :: String -> ConcreteAddr
cAddr = ConcreteAddr . readIP

-- TODO: fix the flowID constructor check
flowID :: String -> FlowID
flowID s | s' >= minBound && s' <= maxBound = s'
         | otherwise                        = flowID "fd00::"
         where s' = fID s

-- TODO: fix the concreteAddr constructor check
concreteAddr :: String -> ConcreteAddr
concreteAddr s | s' >= minBound && s' <= maxBound = s'
               | otherwise                        = concreteAddr "fc00::"
               where s' = cAddr s

checkFlowID :: FlowID -> Bool
checkFlowID f
            | f >= minBound&& f <= maxBound = True
            | otherwise                     = False

checkConcAddr :: ConcreteAddr -> Bool
checkConcAddr a
            | a >= minBound && a <= maxBound = True
            | otherwise                      = False
