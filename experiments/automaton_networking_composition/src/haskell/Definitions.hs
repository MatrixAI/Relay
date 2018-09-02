module Definitions
  where

import qualified Data.ByteString as B
import qualified Data.IP as IP

-- deterministically generated automaton name
type Name = B.ByteString
-- flowID for a communication context
type FlowID = IP.IPv6

-- # Automaton #
-- Generic automaton type used as a template when instantiations are needed. It
-- contains data which will be the same across all automatons.
-- ## - depending on how many FlowIDs we have, it may be necessary to implement
-- ## - a "large automaton" type which has a binary tree instead of a list to
-- ## - facilitate searching.
data Automaton = Automaton {
                    name :: Name,
                    dependencyFlows :: [FlowID],
                    compositionFlows :: [FlowID]
                           } deriving (Show, Eq)

-- # TranslationTable #
-- The table which will be queried when packets arrive in the namespace and need
-- to have addresses translated.
-- ## - naive implementation - list of rules
data TranslationTable = TranslationTable {
                          rules :: [Rule]
                                         } deriving (Show, Eq)

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
data Instance = Instance {
                  automaton :: Automaton,
                  netnsName :: Name,
                  vethNames :: Name,
                  defaultGateway :: IP.IPv6
                         } deriving (Show, Eq)
