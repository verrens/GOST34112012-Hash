module Crypto.Hash.GOST34112012
( GOST34112012Context, hashGOST34112012, 
  hashGOST34112012_256, hashGOST34112012_512, getGOST34112012BufSize,
  initGOST34112012, updateGOST34112012, finishGOST34112012 ) where
import Foreign hiding (newForeignPtr)
import Foreign.Concurrent (newForeignPtr)
import Foreign.C (newCString, CString, CInt(..), CUInt(..), CSize(..))
import Data.ByteString (ByteString, useAsCStringLen, packCStringLen)

#include <gost3411-2012-core.h>

type GOST34112012Context = ForeignPtr GOST34112012Ctx

data GOST34112012Ctx = GOST34112012Ctx {
  cGOST34112012BufSize    :: CSize,
  cGOST34112012DigestSize :: CUInt
}

instance Storable GOST34112012Ctx where
  sizeOf _    = (#size GOST34112012Context)
  alignment _ = (#alignment GOST34112012Context)
  peek ptr    = GOST34112012Ctx
            <$> (#peek GOST34112012Context, bufsize) ptr
            <*> (#peek GOST34112012Context, digest_size) ptr
  poke _ _    = fail "GOST34112012Ctx is read only"

hashGOST34112012_256 :: ByteString -> IO ByteString
hashGOST34112012_256 = hashGOST34112012 256

hashGOST34112012_512 :: ByteString -> IO ByteString
hashGOST34112012_512 = hashGOST34112012 512

hashGOST34112012 :: Int -> ByteString -> IO ByteString
hashGOST34112012 dsize bytes = initGOST34112012 dsize
                   >>= \ctx -> updateGOST34112012 ctx bytes
                            >> finishGOST34112012 ctx

initGOST34112012 :: Int -> IO GOST34112012Context
initGOST34112012 dsize = malloc >>= \ctx -> do
  cGOST34112012Init ctx (fromIntegral dsize)
  newForeignPtr ctx (cGOST34112012Cleanup ctx >> free ctx)

updateGOST34112012 :: GOST34112012Context -> ByteString -> IO ()
updateGOST34112012 ctx bytes = useAsCStringLen bytes (\(str, len) ->
  withForeignPtr ctx (\ptr -> cGOST34112012Update ptr str (fromIntegral len)))

finishGOST34112012 :: GOST34112012Context -> IO ByteString
finishGOST34112012 ctx = withForeignPtr ctx (\ptr -> peek ptr >>= \ct ->
  newCString (replicate (hsize ct) ' ') >>= \str -> cGOST34112012Final ptr str
    >> cGOST34112012Cleanup ptr >> packCStringLen (str, hsize ct)) where
      hsize ct = if 256 == cGOST34112012DigestSize ct then 32 else 64

getGOST34112012BufSize :: GOST34112012Context -> IO Int
getGOST34112012BufSize = fmap (fromIntegral . cGOST34112012BufSize)
                                . (flip withForeignPtr peek)

foreign import ccall "GOST34112012Init"
  cGOST34112012Init :: Ptr GOST34112012Ctx -> CInt -> IO ()

foreign import ccall "GOST34112012Update"
  cGOST34112012Update :: Ptr GOST34112012Ctx -> CString -> CSize -> IO ()

foreign import ccall "GOST34112012Final"
  cGOST34112012Final :: Ptr GOST34112012Ctx -> CString -> IO ()

foreign import ccall "GOST34112012Cleanup"
  cGOST34112012Cleanup :: Ptr GOST34112012Ctx -> IO ()
