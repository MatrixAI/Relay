module Main where

import qualified ParseInput as PI
import qualified SampleNetworking as SN

main :: IO ()
{-
main = SN.createSocket
       >>= \s ->
         putStrLn "How many packets to get?"
         >> getLine
         >>= \x -> SN.getNPackets (read x :: Int) s
-}
main = PI.readUntilEOF []
       >>= \lines -> putStrLn $ show $ PI.parse lines
