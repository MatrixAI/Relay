module ParseInput
--    ( readUntilEOF,
--      parse,
--    ) where
where

import qualified System.IO as SIO
import qualified Data.ByteString.Char8 as C
import Definitions
import qualified Composition1

-- read lines in from STDIN
readUntilEOF :: [String] -> IO [String]
readUntilEOF lns = do
                   iseof <- SIO.hIsEOF SIO.stdin
                   if iseof
                     then return lns
                     else do
                          input <- SIO.hGetLine SIO.stdin
                          readUntilEOF $ lns ++ [input]

--
parse :: [String] -> [[String]]
parse lines = createAutomatons $ map (splitBy ' ') lines
              --map createAutomaton lines[0]

-- splitBy
-- Generic function to split on a delimiter
splitBy :: Eq a => a -> [a] -> [[a]]
splitBy delim list = foldr f [[]] list
        where f curr l@(x:xs)
                | curr == delim = []:l
                | otherwise = (curr:x):xs
