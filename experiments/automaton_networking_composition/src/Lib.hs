{-# LANGUAGE ForeignFunctionInterface #-}

module Lib
    ( someFunc,
      sin,
      -- c_hello,
      -- add
    ) where

import Prelude hiding (sin)
import Foreign.C
import Foreign.Ptr (Ptr, nullPtr)

-- this is a call to C standard library
-- which is always linked in, so we don't need to refer to any file
foreign import ccall "sin" c_sin :: CDouble -> CDouble

sin :: Double -> Double
sin d = realToFrac (c_sin (realToFrac d))

-- this function doesn't exist
-- so it will result in dynamic linking problems
-- foreign import ccall unsafe "hello" c_hello :: IO CInt

-- we need to marshall haskell types to C
-- foreign import ccall "add" c_add :: CInt -> CInt -> CInt
-- add :: Int -> Int -> Int
-- add i1 i2 = fromEnum $ c_add (toEnum i1) (toEnum i2)

someFunc :: IO ()
someFunc = putStrLn "someFunc"
