{-# LANGUAGE        BangPatterns, DeriveGeneric
                  , LambdaCase, OverloadedStrings
                  , DeriveAnyClass, RankNTypes #-}
{-# OPTIONS         -fno-warn-unused-do-bind
                    -fno-warn-unused-imports
                    -fno-warn-orphans
                    -fno-warn-type-defaults    #-}
module Data.Digest.GOST3411 where

import           Control.Exception (throwIO)
import           Control.Monad (when)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.List (genericLength)
import           Foreign hiding (newForeignPtr)
import           Foreign.C (CInt(..), CSize(..), CString, CUChar(..))
import           Foreign.Concurrent (newForeignPtr)

#include <gost3411-2012-core.h>

foreign import ccall "ctx_size"
  ctxSize :: CSize

foreign import ccall "ctx_offset_buffer"
  ctxOffsetBuffer :: CInt

foreign import ccall "ctx_offset_hash"
  ctxOffsetHash :: CInt

foreign import ccall "ctx_offset_h"
  ctxOffsetH :: CInt

foreign import ccall "ctx_offset_N"
  ctxOffsetN :: CInt

foreign import ccall "ctx_offset_Sigma"
  ctxOffsetSigma :: CInt

foreign import ccall "ctx_offset_bufsize"
  ctxOffsetBufsize :: CInt

foreign import ccall "ctx_offset_digest_size"
  ctxOffsetDigestSize :: CInt