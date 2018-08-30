module ParseInput
    ( readUntilEOF,
    ) where

import qualified Control.Monad as CM
import qualified System.IO as SIO

type Name = String
type NetnsName = String
type VethNames = (String, String)
type DefGateway = String
type DepFlows = [String] -- to properly do
type CompositFlow = String -- to properly do
data Automaton = Automaton {
                    name :: Name,
                    netnsName :: NetnsName,
                    vethNames :: VethNames,
                    defaultGateway :: DefGateway,
                    dependencyFlows :: DepFlows,
                    compositionFlow :: CompositFlow
                           } deriving (Show, Eq)

-- read lines in from STDIN
readUntilEOF :: [String] -> IO [String]
readUntilEOF lns = do
                   iseof <- SIO.hIsEOF SIO.stdin
                   if iseof
                     then return lns
                     else do
                          input <- SIO.hGetLine SIO.stdin
                          readUntilEOF $ lns ++ [input]


