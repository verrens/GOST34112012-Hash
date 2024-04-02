{-# LANGUAGE BangPatterns, DeriveGeneric, LambdaCase, OverloadedStrings,
    DeriveAnyClass, RankNTypes, RecordWildCards #-}
{-# OPTIONS         -fno-warn-unused-do-bind
                    -fno-warn-unused-imports
                    -fno-warn-orphans
                    -fno-warn-type-defaults    #-}
module Data.Digest.GOST34112012.Hash where

import Control.Exception (throwIO)
import Control.Monad (when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.List (genericLength)
import Foreign hiding (newForeignPtr)
import Foreign.C (CInt(..), CUInt(..), CSize(..), CString, CUChar(..))
import Foreign.C.String (newCString)
import Foreign.Marshal.Alloc (malloc)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Concurrent (newForeignPtr)

#include <gost3411-2012-core.h>

type GOST34112012HashContext = ForeignPtr GOST34112012HashCtx

initGOST34112012Hash :: Int -> IO GOST34112012HashContext
initGOST34112012Hash digest_size = do
  ctx <- malloc
  cGOST34112012Init ctx $ fromIntegral digest_size
  newForeignPtr ctx $ do
    v <- peek ctx
    when (not $ isCorrectGOST34112012HashCtx v) $
      putStrLn $ " … How many canaries died of boredom? " ++ show v
    cGOST34112012Cleanup ctx
    free ctx

getGOST34112012HashBufSize :: GOST34112012HashContext -> IO Int
getGOST34112012HashBufSize = fmap (fromIntegral . cGOST34112012HashBufSize)
                       . getGOST34112012HashCtx

updateGOST34112012Hash :: GOST34112012HashContext -> ByteString -> IO ()
updateGOST34112012Hash ctx bytes = B.useAsCStringLen bytes $
  \(s, l) -> withForeignPtr ctx $
       \c -> cGOST34112012Update c s $ fromIntegral l

finishGOST34112012Hash :: GOST34112012HashContext -> IO ByteString
finishGOST34112012Hash context = withForeignPtr context $
  \ctx -> peek ctx >>= \case
    c | isCorrectGOST34112012HashCtx c -> do
      rv <- newCString $ replicate 64 ' '
      cGOST34112012Final ctx rv
      cGOST34112012Cleanup ctx
      B.packCString rv
    c | isFinishedGOST34112012HashCtx c -> fail "Finished"
    _ -> fail "Incorrect state"

isFinishedGOST34112012HashCtx :: GOST34112012HashCtx -> Bool
isFinishedGOST34112012HashCtx ctx = let ds = cGOST34112012HashDigestSize ctx
                            in ds == 0

isCorrectGOST34112012HashCtx :: GOST34112012HashCtx -> Bool
isCorrectGOST34112012HashCtx ctx = let ds = cGOST34112012HashDigestSize ctx
                            in ds == 512 || ds == 256 

getGOST34112012HashCtx :: GOST34112012HashContext -> IO GOST34112012HashCtx
getGOST34112012HashCtx = flip withForeignPtr peek

data GOST34112012HashCtx = GOST34112012HashCtx {
  cGOST34112012HashBufSize :: CSize,
  cGOST34112012HashDigestSize :: CUInt
} deriving (Eq, Ord, Show)

instance Storable GOST34112012HashCtx where
  sizeOf _ = (#size GOST34112012Context)
  alignment _ = (#alignment GOST34112012Context)
  peek p = do
    bs <- (#peek GOST34112012Context, bufsize) p
    ds <- (#peek GOST34112012Context, digest_size) p
    pure $ GOST34112012HashCtx bs ds
  poke p GOST34112012HashCtx{..} = pure ()

foreign import ccall "GOST34112012Cleanup"
  cGOST34112012Cleanup :: Ptr GOST34112012HashCtx -> IO ()

foreign import ccall "GOST34112012Init"
  cGOST34112012Init :: Ptr GOST34112012HashCtx -> CInt -> IO ()

foreign import ccall "GOST34112012Update"
  cGOST34112012Update :: Ptr GOST34112012HashCtx -> CString -> CSize -> IO ()

foreign import ccall "GOST34112012Final"
  cGOST34112012Final :: Ptr GOST34112012HashCtx -> CString -> IO ()
