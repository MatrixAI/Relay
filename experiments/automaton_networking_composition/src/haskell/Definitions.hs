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
type Mapping = (FlowID, Automaton)
--
type FlowTable = Map.HashMap FlowID Automaton

-- TODO: gracefully handle invalid IP string inputs
flowID :: String -> FlowID
flowID "" = error "no ipv6 address provided to flowID constructor"
flowID s
        | ip<minIP || ip>maxIP = error "bad flowID"
        | otherwise = read s :: FlowID 
        where ip = read s :: IP.IPv6 
              minIP = read "fd00::" :: IP.IPv6
              maxIP = read "fdff:ffff:ffff:ffff:ffff:ffff:ffff:ffff"
                          :: IP.IPv6

-- TODO: gracefully handle invalid IP string inputs
concreteAddress :: String -> ConcreteAddress
concreteAddress "" = error "no ipv6 address provided to gateway constructor"
concreteAddress s
        | ip<minIP || ip>maxIP = error "bad gateway address"
        | otherwise = read s :: IP.IPv6
        where ip = read s :: IP.IPv6
              minIP = read "fc00::" :: IP.IPv6
              maxIP = read "fcff:ffff:ffff:ffff:ffff:ffff:ffff:ffff"
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
                    dependencyFlows :: [Mapping],
                    compositionFlows :: [Mapping],
                    instances :: [Instance]
                           } deriving (Show, Eq)


-- # Instance #
-- Automaton instance data. 
data Instance = Instance {
                  netnsName :: Name,
                  vethNames :: (Name, Name),
                  defaultGateway :: ConcreteAddress
                         } deriving (Show, Eq)

-- # Composition #
-- The composition of the network. This is a high level model of the network 
data Composition = Composition {
                     automatons :: [Automaton],
                     flowTable :: FlowTable
                               }

-- create a composition mapping between a flowID and the associated
-- destination automaton
compose :: FlowID -> Automaton -> Mapping
compose f a = (f, a)

-- Creates a flow table for the network given a list of automatons
createFlowTable :: [Automaton] -> FlowTable
                    -- [FLowTable] -> FlowTable
createFlowTable [] = newFlowTable
createFlowTable l = foldl unionFlowTable newFlowTable $
                        -- [[Mapping]] -> [FlowTable]
                        map createFlowTable' $
                        -- [Automaton] -> [[Mapping]]
                        map compositionFlows l

-- helper function for creating a flow table given a list of flowID-automaton
-- mappings
createFlowTable' :: [Mapping] -> FlowTable
createFlowTable' [] = newFlowTable
createFlowTable' l = foldl flowTableIns newFlowTable l

-- create an empty flow table
newFlowTable :: FlowTable
newFlowTable = Map.empty

-- merge 2 flowtables
unionFlowTable :: FlowTable -> FlowTable -> FlowTable
unionFlowTable a b = Map.union a b

-- 
flowTableIns :: FlowTable -> (FlowID, Automaton) -> FlowTable
flowTableIns m (f,a)
        | null m = Map.singleton f a
        | exists/=Nothing = error "FlowID in use in translation table"
        | otherwise = Map.insert f a m
        where exists = Map.lookup f m 

-- # hashAndHex #
-- Takes a string, hashes it and then shows it in hex.
hashAndHex :: String -> String
hashAndHex s = take 15 $ show $ H.hashWith H.SHA1 $ pack s
