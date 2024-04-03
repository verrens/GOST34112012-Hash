module Data.Digest.GOST34112012.Hash
( GOST34112012HashContext, hashGOST34112012, getGOST34112012HashBufSize,
  initGOST34112012Hash, updateGOST34112012Hash, finishGOST34112012Hash ) where
import Foreign hiding (newForeignPtr)
import Foreign.Concurrent (newForeignPtr)
import Foreign.C (newCString, CString, CInt(..), CUInt(..), CSize(..))
import Data.ByteString (ByteString, useAsCStringLen, packCStringLen)

#include <gost3411-2012-core.h>

type GOST34112012HashContext = ForeignPtr GOST34112012HashCtx

data GOST34112012HashCtx = GOST34112012HashCtx {
  cGOST34112012HashBufSize    :: CSize,
  cGOST34112012HashDigestSize :: CUInt
} deriving (Eq, Ord, Show)

instance Storable GOST34112012HashCtx where
  sizeOf _    = (#size GOST34112012Context)
  alignment _ = (#alignment GOST34112012Context)
  peek ptr    = GOST34112012HashCtx
            <$> (#peek GOST34112012Context, bufsize) ptr
            <*> (#peek GOST34112012Context, digest_size) ptr
  poke _ _    = fail "GOST34112012HashCtx is read only"

hashGOST34112012 :: Int -> ByteString -> IO ByteString
hashGOST34112012 dsize bytes = initGOST34112012Hash dsize
                   >>= \ctx -> updateGOST34112012Hash ctx bytes
                            >> finishGOST34112012Hash ctx

initGOST34112012Hash :: Int -> IO GOST34112012HashContext
initGOST34112012Hash dsize = malloc >>= \ctx -> do
  cGOST34112012Init ctx (fromIntegral dsize)
  newForeignPtr ctx (cGOST34112012Cleanup ctx >> free ctx)

updateGOST34112012Hash :: GOST34112012HashContext -> ByteString -> IO ()
updateGOST34112012Hash ctx bytes = useAsCStringLen bytes (\(str, len) ->
  withForeignPtr ctx (\ptr -> cGOST34112012Update ptr str (fromIntegral len)))

finishGOST34112012Hash :: GOST34112012HashContext -> IO ByteString
finishGOST34112012Hash ctx = withForeignPtr ctx (\ptr -> peek ptr >>= \ct ->
  newCString (replicate (hsize ct) ' ') >>= \str -> cGOST34112012Final ptr str
    >> cGOST34112012Cleanup ptr >> packCStringLen (str, hsize ct)) where
      hsize ct = if 256 == cGOST34112012HashDigestSize ct then 32 else 64

getGOST34112012HashBufSize :: GOST34112012HashContext -> IO Int
getGOST34112012HashBufSize = fmap (fromIntegral . cGOST34112012HashBufSize)
                                . (flip withForeignPtr peek)

foreign import ccall "GOST34112012Init"
  cGOST34112012Init :: Ptr GOST34112012HashCtx -> CInt -> IO ()

foreign import ccall "GOST34112012Update"
  cGOST34112012Update :: Ptr GOST34112012HashCtx -> CString -> CSize -> IO ()

foreign import ccall "GOST34112012Final"
  cGOST34112012Final :: Ptr GOST34112012HashCtx -> CString -> IO ()

foreign import ccall "GOST34112012Cleanup"
  cGOST34112012Cleanup :: Ptr GOST34112012HashCtx -> IO ()
