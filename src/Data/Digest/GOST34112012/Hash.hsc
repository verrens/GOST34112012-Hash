module Data.Digest.GOST34112012.Hash where
import Foreign hiding (newForeignPtr)
import Foreign.Concurrent (newForeignPtr)
import Foreign.C (newCString, CInt(..), CUInt(..), CSize(..), CString)
import Data.ByteString (ByteString, useAsCStringLen, packCString)
import Control.Monad (when)

#include <gost3411-2012-core.h>

type GOST34112012HashContext = ForeignPtr GOST34112012HashCtx

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
  poke _ _ = fail "One-way communication"

digestSizes :: [Int]
digestSizes = [512, 256]

hashGOST34112012 :: Int -> ByteString -> IO ByteString
hashGOST34112012 digest_size bytes = initGOST34112012Hash digest_size
                         >>= \ctx -> updateGOST34112012Hash ctx bytes
                                  >> finishGOST34112012Hash ctx

initGOST34112012Hash :: Int -> IO GOST34112012HashContext
initGOST34112012Hash digest_size = malloc >>= \ctx -> do
  when (notElem digest_size digestSizes) $
    fail $ "Supported digest sizes: " ++ show digestSizes
  cGOST34112012Init ctx $ fromIntegral digest_size
  newForeignPtr ctx $ peek ctx >>= flip when (cGOST34112012Cleanup ctx)
                                 . isCorrectGOST34112012HashCtx >> free ctx

updateGOST34112012Hash :: GOST34112012HashContext -> ByteString -> IO ()
updateGOST34112012Hash ctx bytes = useAsCStringLen bytes $ \(s, l) ->
  withForeignPtr ctx $ \c -> cGOST34112012Update c s $ fromIntegral l

finishGOST34112012Hash :: GOST34112012HashContext -> IO ByteString
finishGOST34112012Hash context = withForeignPtr context $
  \ctx -> peek ctx >>= \c -> if isCorrectGOST34112012HashCtx c then do
      v <- newCString $ replicate 64 ' '
      cGOST34112012Final ctx v >> cGOST34112012Cleanup ctx >> packCString v
    else fail $ "Unexpected state: " ++ show c

getGOST34112012HashBufSize :: GOST34112012HashContext -> IO Int
getGOST34112012HashBufSize = fmap (fromIntegral . cGOST34112012HashBufSize)
                                . (flip withForeignPtr peek)

isCorrectGOST34112012HashCtx :: GOST34112012HashCtx -> Bool -- TODO
isCorrectGOST34112012HashCtx ctx = let ds = cGOST34112012HashDigestSize ctx
                                   in elem ds $ map fromIntegral digestSizes

foreign import ccall "GOST34112012Cleanup"
  cGOST34112012Cleanup :: Ptr GOST34112012HashCtx -> IO ()

foreign import ccall "GOST34112012Init"
  cGOST34112012Init :: Ptr GOST34112012HashCtx -> CInt -> IO ()

foreign import ccall "GOST34112012Update"
  cGOST34112012Update :: Ptr GOST34112012HashCtx -> CString -> CSize -> IO ()

foreign import ccall "GOST34112012Final"
  cGOST34112012Final :: Ptr GOST34112012HashCtx -> CString -> IO ()
