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
  ConcreteInstance (..), Automaton (..)
) where

import Data.Hashable
import qualified Data.HashMap.Strict as Map
import qualified Data.IP as IP

instance Hashable IP.IPv6

-- deterministically generated automaton name
type Name = String
-- flowID for a communication context
-- flowIDs are of the IPv6 range fd00::/7 -> unique-local range
type FlowID = IP.IPv6
-- concrete address of an automaton
-- concrete addresses are of the IPv6 range fc00::7 -> unique-local range
type ConcreteAddress = IP.IPv6
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
                          netns :: Name,
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
