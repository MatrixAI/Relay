module ParseInput
    ( readUntilEOF,
      parse
    ) where

import qualified System.IO as SIO
import Definitions

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
--parse :: [String] ->
--parse lines =  

--
splitOn :: Char -> String ->[String]
splitOn _ "" = [""]
splitOn delim string = foldr f [[]]
              where f char line@(x:xs)
                      | char == delim = []:line
                      | otherwise = (char:x):xs
