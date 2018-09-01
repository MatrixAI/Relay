module ParseInput
    ( readUntilEOF,
    ) where

import qualified Control.Monad as CM
import qualified System.IO as SIO

-- deterministically generated automaton name
type Name = String
-- randomly generated network namespace name
-- will be different for each instance of an automaton
type NetnsName = String
-- randomly generated veth endpoint names
-- will be different for each instance of an automaton
type VethNames = (String, String)
-- default gateway address within the network namespace
type DefaultGateway = String
-- flow addresses for automatons which are being depended upon
-- TODO
type DependencyFlows = [String]
-- flow address for the automaton composition
-- TODO
type CompositionFlow = String

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


