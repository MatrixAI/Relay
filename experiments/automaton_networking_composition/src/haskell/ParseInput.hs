module ParseInput
    ( readUntilEOF,
    ) where

import qualified Control.Monad as CM
import qualified System.IO as SIO

-- read lines in from STDIN
readUntilEOF :: [String] -> IO [String]
readUntilEOF lns = do
                   iseof <- SIO.hIsEOF SIO.stdin
                   if iseof
                     then return lns
                     else do
                          input <- SIO.hGetLine SIO.stdin
                          readUntilEOF $ lns ++ [input]


