{-
 - Functions to operate on these models are found in another file.
 - 
 - ramwan <ray.wan@matrix.ai>
 -}

module Definitions (
  Name,
  FlowID,
  ConcreteAddress,
  Mapping,
  FlowTable,
  ConcreteInstance,
  AbstractInstance,
  Automaton,
  Composition
) where

import Data.Hashable
import qualified Data.HashMap.Strict as Map
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import qualified Data.IP as IP
import qualified Data.Matrix as M

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
--
type CompositionGraph = M.Matrix Int


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
 - Contains data related to the functionality of the automaton in relation to
 - the Relay network.
 -}
data AbstractInstance = AbstractInstance {
                          dependencyFlows :: [FlowID],
                          compositionFlows :: [FlowID],
                          concreateInstance :: ConcreteInstance
                                         }

{-
 - Generic Automaton structure containing high level information about the
 - automaton.
 -}
data Automaton = Automaton {
                   index :: Int,
                   name :: Name
                           }

{-
 - Data structure containing static information about the composition of the
 - network. This structure should not change unless the network structure needs
 - to be changed.
 -}
data Composition = Composition {
                     automatons :: [Automaton],
                     abstractInstances :: [AbstractInstance],
                     compositionGraph :: CompositionGraph
                               }
