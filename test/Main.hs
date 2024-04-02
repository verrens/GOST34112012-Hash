{-# LANGUAGE OverloadedStrings #-}
module Main (main) where
import Data.Digest.GOST34112012.Hash
import Prelude hiding (readFile)
import Data.ByteString.Base16 (encode)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Text (Text, toUpper)
import Data.Text.IO (readFile)
import Control.Monad (when)

main :: IO ()
main = do
  hashB16 512 "" >>= test "empty" emptyB16_512
  hashB16 512 "xe" >>= test "two ascii" twoAsciiB16_512
  hashB16 512 "хе" >>= test "two unicode" twoUnicodeB16_512
  readFile "LICENSE" >>= hashB16 512 >>= test "LICENSE" licenseB16_512
  pure ()

emptyB16_512 :: Text
emptyB16_512 = "8E945DA209AA869F0455928529BCAE4679E9873AB707B55315F56CEB98BEF0A7362F715528356EE83CDA5F2AAC4C6AD2BA3A715C1BCD81CB8E9F90BF4C1C1A8A"

twoAsciiB16_512 :: Text
twoAsciiB16_512 = toUpper "f5e2ec8bf5705a0b6ed1afd6e469a3cabd1a0476d8ae904aafdb094c69b33f5f2a890b9258c205dde383b83a1a391f18685b4395de4b4d1037944448ce797068"

twoUnicodeB16_512 :: Text
twoUnicodeB16_512 = toUpper "c90966f0594acbfec047b11b02d03344e19070e35b36c523577223aeae0ad8d3172d8955174faac47a256628ac7e69427222ace2eae454ea7fa52086ee43fa97"

licenseB16_512 :: Text
licenseB16_512 = toUpper "2c7302f5d67762f50d49fa7ddf09a11516a385dc26831152df4d8f1928662a6fe90c786705584010ddd4822a9ef3b119524d5aaaedab7c6c84a26990961ece18"

test :: Eq a => String -> a -> a -> IO ()
test msg tst = flip when (fail msg) . (/= tst)

hashB16 :: Int -> Text -> IO Text
hashB16 bs = fmap (toUpper . decodeUtf8 . encode) . hashGOST34112012 bs . encodeUtf8
