module Definitions
  where

import qualified Data.IP as IP

-- deterministically generated automaton name
type Name = ByteString
-- randomly generated network namespace name
-- will be different for each instance of an automaton
type NetnsName = ByteString
-- randomly generated veth endpoint names
-- will be different for each instance of an automaton
type VethNames = (ByteString, ByteString)
-- default gateway address within the network namespace
type DefaultGateway = IP.IPv6
-- flow addresses for automatons which are being depended upon
type DependencyFlows = [IP.IPv6]
-- flow address for the automaton composition
type CompositionFlow = IP.IPv6

data Automaton = Automaton {
                    name :: Name,
                    netnsName :: NetnsName,
                    vethNames :: VethNames,
                    defaultGateway :: DefaultGateway,
                    dependencyFlows :: DependencyFlows,
                    compositionFlow :: CompositionFlow
                           } deriving (Show, Eq)
