{-
 - Simple resource counter.
 -
 - ramwan <ray.wan@matrix.ai>
 -}

module Counter (
  allocate,
  netnsName, vethNames
) where

import Control.Monad.State (State, state, runState)
import System.Random
import DataDefinitions

netnsNameLength = 20
vethNameLength = 15

-- no tracking of deallocated resources
-- failure is NOT checked...
allocate :: Enum a => State a a
allocate = state $ \n -> (n, succ n)


-- no tracking of allocated names
-- failure is NOT checked
netnsName :: RandomGen g => State g Name
netnsName = state $ \g -> let
                            (n, g') = randNIfChar netnsNameLength g
                            n' = map (\(IFChar c) -> c) n
                          in (n', g')

-- no tracking of allocated names
-- failure is NOT checked
vethNames :: RandomGen g => State g (Name, Name)
vethNames = state $ \g -> let
                            (n1, g1) = runState vethName' g
                            (n2, g2) = runState vethName' g1
                          in ((n1, n2), g2)

-- no trackin gof allocated names
-- failure is NOT checked
vethName' :: RandomGen g => State g Name
vethName' = state $ \g -> let
                            (n, g') = randNIfChar vethNameLength g
                            n' = map (\(IFChar c) -> c) n
                          in (n', g')





randNIfChar :: (Eq n, Num n, RandomGen g) =>
               n -> g -> ([IFChar], g)
randNIfChar = randN

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
randRN :: (Eq n, Num n, Random a, RandomGen g) => (a, a) -> n -> g -> ([a], g)
randRN ival n g = (map fst l, snd $ last l)
                  where l = buildRandoms (randomR ival) n g

randN :: (Eq n, Num n, Random a, RandomGen g) => n -> g -> ([a], g)
randN n g = (map fst l, snd $ last l)
            where l = buildRandoms random n g
