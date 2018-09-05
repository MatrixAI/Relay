module Definitions
  where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import qualified Data.HashMap.Strict as Map
import qualified Data.IP as IP
import qualified Data.Char as C
import Data.Hashable
import qualified Crypto.Hash as H

instance Hashable IP.IPv6
-- deterministically generated automaton name
type Name = String
-- flowID for a communication context
-- flowIDs are of the IPv6 range fd00::/7 -> unique-local range
type FlowID = IP.IPv6
-- concrete address of an automaton
-- concrete addresses are of the IPv6 range fc00::7 -> unique-local range
type ConcreteAddress = IP.IPv6
--
type FlowTable = Map.HashMap FlowID Automaton

--
flowID :: String -> FlowID
flowID "" = error "no ipv6 address provided to flowID constructor"
flowID s
        | ip<minBound || ip>maxBound = error "bad flowID"
        | otherwise = read s :: FlowID 
        where ip = read s :: IP.IPv6 
              minBound = read "fd00::" :: IP.IPv6
              maxBound = read "fdff:ffff:ffff:ffff:ffff:ffff:ffff:ffff"
                          :: IP.IPv6

--
concreteAddress :: String -> ConcreteAddress
concreteAddress "" = error "no ipv6 address provided to gateway constructor"
concreteAddress s
        | ip<minBound || ip>maxBound = error "bad gateway address"
        | otherwise = read s :: IP.IPv6
        where ip = read s :: IP.IPv6
              minBound = read "fc00::" :: IP.IPv6
              maxBound = read "fcff:ffff:ffff:ffff:ffff:ffff:ffff:ffff"
                          :: IP.IPv6

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
                    compositionFlows :: [(FlowID, Automaton)],
                    instances :: [Instance]
                           } deriving (Show, Eq)

-- # Instance #
-- Automaton instance data. 
-- netnsName - randomly generated network namespace name
--             will be different for each instance of an automaton
-- vethNames - randomly generated veth endpoint names
--             will be different for each instance of an automaton
data Instance = Instance {
                  netnsName :: Name,
                  vethNames :: [Name],
                  defaultGateway :: ConcreteAddress
                         } deriving (Show, Eq)

-- # Composition #
data Composition = Composition {
                     automatons :: [Automaton],
                     flowTable :: FlowTable
                               }

-- # hashToHex #
-- Takes a string, hashes it and then shows it in hex.
hashToHex :: String -> String
hashToHex s = take 15 $ show $ H.hashWith H.SHA1 $ pack s

newFlowTable :: FlowTable
newFlowTable = Map.empty

--
flowTableIns :: FlowTable -> (FlowID, Automaton) -> FlowTable
flowTableIns m (f,a)
        | null m = Map.singleton f a
        | exists/=Nothing = error "FlowID in use in translation table"
        | otherwise = Map.insert f a m
        where exists = Map.lookup f m 
