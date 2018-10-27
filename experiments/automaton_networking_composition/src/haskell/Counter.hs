{-
 - Simple resource counter.
 -
 - ramwan <ray.wan@matrix.ai>
 -}

module Counter (
  allocate,
--  netnsName, vethNames
) where

import Control.Monad.State (State, state)
import System.Random
import DataDefinitions

-- no tracking of deallocated resources
-- failure is NOT checked...
allocate :: Enum a => State a a
allocate = state $ \n -> (n, succ n)

{-
-- no tracking of allocated names
-- State Name g == g -> (Name, g)
netnsName :: State Name g
netnsName = state $ -- placeholder

-- no tracking of allocated names
vethNames :: State (Name, Name) g
vethNames = do state ((n1, n2), g2)
               where (n1, g1) = vethName' g
                     (n2, g2) = vethName' g1

vethName' :: State Name g
vethName' = state $ \g -> (1, g)-- placeholder
-}

-- Defined in System.Random module but not exported.
--
-- ghc ticket #4218 says that System.Random is too lazy
-- https://ghc.haskell.org/trac/ghc/ticket/4218
-- hence the use of `seq` to make it less lazy
buildRandoms :: (Eq n, Num n, RandomGen g) =>
                (g -> (a, g)) -> n -> g -> [(a, g)]
buildRandoms randf 1 g =
            x `seq` ((x, g'):[]) where (x, g') = randf g
buildRandoms randf n g =
            x `seq` ((x, g'):(buildRandoms randf (n-1) g'))
            where (x, g') = randf g

-- Generates a list (of specified length) and returns the updated generator
randN :: (Eq n, Num n, Random a, RandomGen g) =>
            (a, a) -> n -> g -> ([a], g)
randN ival n g =
          (map fst l, snd $ last l)
          where l = buildRandoms (randomR ival) n g
