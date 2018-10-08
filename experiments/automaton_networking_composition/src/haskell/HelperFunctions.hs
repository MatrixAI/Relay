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

-- # hashAndHex #
-- Takes a string, hashes it and then shows it in hex.
-- 15 is an arbitrary number
hashAndHex :: String -> String
hashAndHex s = take 15 $ show $ H.hashWith H.SHA1 $ pack s
