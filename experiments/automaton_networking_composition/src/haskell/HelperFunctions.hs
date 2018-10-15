module HelperFunctions (
  readIP
)  where

import Data.IP (IPv6)

-- TODO: error handle
readIP :: String -> IPv6
readIP s = read s :: IPv6
