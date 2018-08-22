{-# LANGUAGE ForeignFunctionInterface #-}

import Prelude hiding (sin)
import Foreign.C
import Foreign.Ptr (Ptr, nullptr)

-- this is a call to C standard library
-- -- which is always linked in, so we don't need to refer to any file
foreign import ccall "sin" c_sin :: CDouble -> CDouble

sin :: Double -> Double
sin d = realToFrac (c_sin (realToFrac d))
