module Main where

import qualified ParseInput as PI
import qualified SampleNetworking as SN

main :: IO ()
main = SN.createSocket
       >>= SN.getPacket
