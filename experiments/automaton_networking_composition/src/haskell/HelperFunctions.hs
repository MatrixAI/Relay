module HelperFunctions (
  readIP,
  buildRandoms,
  randRN,
  randN
)  where

import Data.IP (IPv6)
import System.Random

-- TODO: error handle
readIP :: String -> IPv6
readIP s = read s :: IPv6



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
