module HelperFunctions
  where

import Data.Hashable
import qualified Crypto.Hash as H
import qualified Data.HashMap.Strict as Map
import qualified Data.IP as IP
import Data.ByteString.Char8 (pack)

import Definitions

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
