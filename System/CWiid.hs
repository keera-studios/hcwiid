{-# LANGUAGE ForeignFunctionInterface #-}
module System.CWiid
       (c_cwiid_open
       ) where

import Prelude
import Foreign
import Foreign.C
import Foreign.C.Error
import Foreign.Marshal
import Foreign.Ptr (Ptr)

foreign import ccall unsafe "cwiid_open" c_cwiid_open
  :: Ptr a -> Int -> IO (Ptr b)
