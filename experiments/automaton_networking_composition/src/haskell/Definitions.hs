module Definitions
  where

import Data.ByteString (ByteString)
import qualified Data.HashMap.Strict as Map
import qualified Data.IP as IP
import qualified Data.Char as C
import qualified Crypto.Hash as H
import qualified Numeric as N

-- deterministically generated automaton name
type Name = String
-- flowID for a communication context
-- flowIDs are of the IPv6 range fd00::/7 -> unique-local range
type FlowID = IP.IPv6

-- # Automaton #
-- Generic automaton type used as a template when instantiations are needed. It
-- contains data which will be the same across all automatons.
-- ## - depending on how many FlowIDs we have, it may be necessary to implement
-- ## - a "large automaton" type which has a binary tree instead of a list to
-- ## - facilitate searching.
data Automaton = Automaton {
                    name :: Name,
                    numberInstances :: Int,
                    dependencyFlows :: [FlowID],
                    compositionFlows :: [FlowID]
                    instances :: [Instance]
                           } deriving (Show, Eq)

-- # Generic Table structure #
-- ## - naive implementation - long tree
data Table a = End | Entry a (Table a) deriving (Show, Eq)

-- # Rule #
-- Translation rule to be matched against.
data Rule = Rule {
              toFlowID :: FlowID,
              toAutomaton :: Automaton
                 } deriving (Show, Eq)

-- # Instance #
-- Automaton instance data. 
-- netnsName - randomly generated network namespace name
--             will be different for each instance of an automaton
-- vethNames - randomly generated veth endpoint names
--             will be different for each instance of an automaton
-- defaultGateway - will be an IPv6 address of format fc00::/7
--                  -> unique-local address
data Instance = Instance {
                  netnsName :: Name,
                  vethNames :: [Name],
                  defaultGateway :: IP.IPv6
                         } deriving (Show, Eq)

-- # hashToHex #
-- Takes a string, hashes it and then shows it in hex.
hashToHex :: String -> String
hashToHex s = take 15 $ show $ H.hashWith H.SHA1 s

-- tableInsert                                                                  
-- Generic function to insert on Table datatype                                 
tableInsert :: a -> Table a -> Table a                                          
tableInsert e End = Entry e End                                                 
tableInsert e (Entry x table) = Entry x (tableInsert e table)
