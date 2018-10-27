{-
 - Functions to operate on these models are found in another file.
 - 
 - ramwan <ray.wan@matrix.ai>
 -}

module DataDefinitions (
  Name, IFChar,
  flowID, concreteAddr,
  Mapping,
  FlowTable,
  ConcreteInstance (..), Automaton (..),
  checkFlowID, checkConcAddr
) where

import Data.Hashable
import qualified Data.HashMap.Strict as Map
import Data.IP (IPv6)
import System.Random (Random)
import Data.Maybe (fromJust)

import HelperFunctions (readIP)

-- deterministically generated automaton name
type Name = String

-- Alternative Typeclass
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
{-
instance Random IFChar where
  randomR = 
  random = 
-}


-- flowID for a communication context
-- flowIDs are of the IPv6 range fd00::/7 -> unique-local range
-- TODO: newtype FlowID and ConcreteAddr so we can create unique Bounded
-- instances
newtype FlowID = FlowID IPv6 deriving (Eq, Show, Ord)
instance Bounded FlowID where
  minBound = fID "fd00::"
  maxBound = fID "fdff:ffff:ffff:ffff:ffff:ffff:ffff:ffff"



-- concrete address of an automaton
-- concrete addresses are of the IPv6 range fc00::7 -> unique-local range
newtype ConcreteAddr = ConcreteAddr IPv6 deriving (Eq, Show, Ord)
instance Bounded ConcreteAddr where
  minBound = cAddr "fc00::"
  maxBound = cAddr "fcff::ffff:ffff:ffff:ffff:ffff:ffff:ffff"



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
                          -- 256 bytes including trailing NULL
                          netns :: Name,
                          -- IF namesize is 16 bytes with trailing NULL
                          veths :: (Name, Name),
                          gateway :: ConcreteAddr
                                         }

{-
 - Generic Automaton structure containing high level information about the
 - automaton.
 -
 - TODO: perform checking on `instances` to make sure it's >= 0
 -}
data Automaton = Automaton {
                   name :: Name,
                   instances :: Int
                           } deriving (Show)

-- Helper functions
fID   s = FlowID $ readIP s
cAddr s = ConcreteAddr $ readIP s

-- TODO: fix the flowID constructor check
flowID :: String -> FlowID
flowID s | let s' = fID s in
           s' >= (minBound :: FlowID) ||
           s' <= (maxBound :: FlowID)     = fID s
         | otherwise                      = flowID "fd00::"

-- TODO: fix the concreteAddr constructor check
concreteAddr :: String -> ConcreteAddr
concreteAddr s | let s' = cAddr s in
                 s' >= (minBound :: ConcreteAddr) ||
                 s' <= (maxBound :: ConcreteAddr)    = cAddr s
               | otherwise                           = concreteAddr "fc00::"

checkFlowID :: FlowID -> Bool
checkFlowID f
            | f >= (minBound :: FlowID) ||
              f <= (maxBound :: FlowID)     = True
            | otherwise                     = False

checkConcAddr :: ConcreteAddr -> Bool
checkConcAddr a
            | a >= (minBound :: ConcreteAddr) ||
              a <= (maxBound :: ConcreteAddr)     = True
            | otherwise                           = False
