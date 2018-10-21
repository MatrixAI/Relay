{-
 - Simple resource counter.
 -
 - ramwan <ray.wan@matrix.ai>
 -}

module Counter (
  allocate,
  netnsName, vethNames
) where

import Control.Monad.State (State, state)
import System.Random
import DataDefinitions

-- no tracking of deallocated resources
-- failure is NOT checked...
allocate :: Enum a => State a a
allocate = state $ \n -> (n, succ n)

-- ISSUE: randomRs doesn't return a new generator

-- no tracking of allocated names
netnsName :: State Name g
netnsName = state $ \g -> take 20 $ randomRs ('a', 'z') g

-- no tracking of allocated names
vethNames :: State (Name, Name) g
vethNames = do return ((n1, n2), g2)
              where (n1, g1) = vethName' g
                    (n2, g2) = vethName' g1

vethName' :: State Name g
vethName' = state $ \g -> take 15 $ randomRs ('A', 'Z') g
