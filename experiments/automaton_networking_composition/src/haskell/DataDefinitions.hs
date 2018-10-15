{-
 - Functions to operate on these models are found in another file.
 - 
 - ramwan <ray.wan@matrix.ai>
 -}

module DataDefinitions (
  Name,
  FlowID, ConcreteAddress,
  Mapping,
  FlowTable,
  ConcreteInstance (..), Automaton (..),
  minFlowID, maxFlowID,
  minConcreteAddress, maxConcreteAddress,
  Stack, CounterState (..),
  checkFlowID, checkCAddress
) where

import Data.Hashable
import qualified Data.HashMap.Strict as Map
import Data.IP (IPv6)

import HelperFunctions (readIP)

-- deterministically generated automaton name
type Name = String
-- flowID for a communication context
-- flowIDs are of the IPv6 range fd00::/7 -> unique-local range
type FlowID = IPv6
-- concrete address of an automaton
-- concrete addresses are of the IPv6 range fc00::7 -> unique-local range
type ConcreteAddress = IPv6
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
                          gateway :: ConcreteAddress
                                         }

{-
 - Generic Automaton structure containing high level information about the
 - automaton.
 -}
data Automaton = Automaton {
                   name :: Name,
                   instances :: Int
                           } deriving (Show)


{-
 - Data types and functions to keep track of resource allocation
 -}
type Stack c = [c]
data CounterState c = CState c (Stack c) deriving (Show)

minFlowID = readIP "fd00::"
maxFlowID = readIP "fdff:ffff:ffff:ffff:ffff:ffff:ffff:ffff"
minConcreteAddress = readIP "fc00::"
maxConcreteAddress = readIP "fcff::ffff:ffff:ffff:ffff:ffff:ffff:ffff"

checkFlowID :: FlowID -> Bool
checkFlowID f
            | f >= minFlowID || f <= maxFlowID = True
            | otherwise = False

checkCAddress :: ConcreteAddress -> Bool
checkCAddress a
            | a >= minConcreteAddress || a <= maxConcreteAddress = True
            | otherwise = False


