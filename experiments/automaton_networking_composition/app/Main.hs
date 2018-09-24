module Main where

import qualified ParseInput as PI
import qualified Networking as N

main :: IO ()
{-
main = SN.createSocket
       >>= \s ->
         putStrLn "How many packets to get?"
         >> getLine
         >>= \x -> SN.getNPackets (read x :: Int) s
-}
main = putStrLn "Nothing in main yet..."
