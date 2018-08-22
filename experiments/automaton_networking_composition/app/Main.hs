module Main where

import qualified ParseInput as PI

main :: IO ()
main = PI.readUntilEOF []
       >>= print
