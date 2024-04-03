{-# LANGUAGE OverloadedStrings #-}
module Main (main) where
import Data.Digest.GOST34112012.Hash
import Prelude hiding (readFile, putStrLn)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BIO
import Data.ByteString.Base16 (encode)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Text (Text, toUpper, pack, unpack)
import Data.Text.IO (putStrLn)
import qualified Data.Text.IO as TIO
import Control.Monad (when, forM_)
import System.CPUTime (getCPUTime, cpuTimePrecision)
import Control.Exception (catchJust)
import System.IO.Error (ioeGetErrorType, isDoesNotExistErrorType)

main :: IO ()
main = do
  t0 <- getCPUTimeNS

  hashB16 512 "" >>= test "empty" emptyB16_512
  
  hashB16 512 "xe" >>= test "two ascii" twoAsciiB16_512
  hashB16 512 "хе" >>= test "two unicode" twoUnicodeB16_512

  TIO.readFile "LICENSE" >>=
    hashB16 512 >>= test "LICENSE" licenseB16_512
  TIO.readFile "LICENSE" >>=
    hashB16 256 >>= test "LICENSE" licenseB16_256
  TIO.readFile "c_src/streebog/LICENSE.GPL2" >>=
    hashB16 256 >>= test "LICENSE.GPL2" licenseB16_256

  forM_ [ (bs, i) | i <- [1, 3, 4, 5, 6], bs <- [512, 256] ] $ uncurry $
    testM t0 TIO.readFile hashB16
    
  testM t0 BIO.readFile hashB16B 512 2 -- win-1251 
  testM t0 BIO.readFile hashB16B 256 2

-- 4G file! Use make4G if needed
  catchJust (\e -> if isDoesNotExistErrorType (ioeGetErrorType e) then Just () else Nothing)
            ( testM t0 BIO.readFile hashB16B 512 7 >>
              testM t0 BIO.readFile hashB16B 256 7)
            (\_ -> putStrLn
    "Skiping etalon M7. The file takes about 4 gbytes, and can be generated by the make4Gb command.")

  testM t0 BIO.readFile hashB16B 512 8 -- carry
  testM t0 BIO.readFile hashB16B 256 8

  pure ()

etalonPath :: FilePath
etalonPath = "test/etalon/"

emptyB16_512 :: Text
emptyB16_512 = "8E945DA209AA869F0455928529BCAE4679E9873AB707B55315F56CEB98BEF0A7362F715528356EE83CDA5F2AAC4C6AD2BA3A715C1BCD81CB8E9F90BF4C1C1A8A"

twoAsciiB16_512 :: Text
twoAsciiB16_512 = toUpper "f5e2ec8bf5705a0b6ed1afd6e469a3cabd1a0476d8ae904aafdb094c69b33f5f2a890b9258c205dde383b83a1a391f18685b4395de4b4d1037944448ce797068"

twoUnicodeB16_512 :: Text
twoUnicodeB16_512 = toUpper "c90966f0594acbfec047b11b02d03344e19070e35b36c523577223aeae0ad8d3172d8955174faac47a256628ac7e69427222ace2eae454ea7fa52086ee43fa97"

licenseB16_512 :: Text
licenseB16_512 = toUpper "7ECACDDC1E3C92DBE2CDA16F2B035BD5278CDA5AB248F94117853C900105295D15490175F1C07D38D235A48FD9C165E01673BAB76A12BB9C924DA752189AEB9B"

licenseB16_256 :: Text
licenseB16_256 = "972B18C6ABA96CBADB6C1F817DFB7CBCA2E08BCA5513819E05CEF85B69A7E1CC"

etalonsCheck :: String -> Text -> Bool
etalonsCheck tid csum = maybe False (== csum) $ lookup tid [
  ("1.512", "D7630524FCAF7054613064EE5FE1D19885FCD33B2F74FB074F1A6724420E127F"),
  ("1.256", "C81762568A91969C2FFBC4EE9F4817B636043BFA95A9A25FBCDBAF4ADB8D3ACE"),
  ("2.512", "35A4B0011506B6521A7222FECAF2E97F6C3482F55ABA4F821D6302E8EF553551"),
  ("2.256", "9F86EABE1AC4DEC3B46378DB6470ABEEFE9AEADE12116A269875F391C2B9763E"),
  ("3.512", "1B101143197C9B5CFAC2F2D9A0A29BE8315B76FF442BFA8C7003522CF6BF76EF"),
  ("3.256", "D789EC569F4910EB3CCF37D04C4EAE9119CA8BF50F10E4CC8D4DB8ED641374B5"),
  ("4.512", "5A269B8F9126EF8233F9255EC30E41295692294DC0F52584BC01EF91C0315BCD"),
  ("4.256", "DFC267C06977089381D049BF3532862CBBA81991079FA6706AC66C3A2E6E1803"),
  ("5.512", "6E543523B6BE13F3F516D3B6F4C29D66A3E57F562F494AACD2BCD9BDA641B20B"),
  ("5.256", "502BD3C103F5C751F09E8129AFF7445CD3036F16182883773964A2DDFF42FC29"),
  ("6.512", "F3A1BFBE060CE023343549259840BAD60A5718A7996911E9DFDBCFD4698FD79F"),
  ("6.256", "CBF20C9196004329067882AE468E8B850600E23687CD846419E38B9D18123566"),
  ("7.256", "6AC9BCEC2F70243C9E55331E553BB351266B638085A46F7EFF3D053FD1B1EA7C"),
  ("7.512", "18C144CED8D060D8D2AB933B8EF7D30973E6ECB3E02C548804A29A0CB616DE46"),
  ("8.512", "6F2ED35F3541D12C25F3C7B3FC049DD3053E4F2687331C86FB907BA132D01A17"),
  ("8.256", "AAAD2B63E972EA3EAF14922255786A56ECBC9DFF7589C81C40DC96F52F3695EA")
  ]

testM :: Integer -> (FilePath -> IO t) -> (Int -> t -> IO Text) -> Int -> Int -> IO ()
testM t0 r h bs n = do
  t <- getCPUTimeNS
  putStrLn $ pack $ "[" ++ show (t - t0) ++ "] Processing etalon " ++ show tid
  chk <- TIO.readFile $ etalonPath ++ "H" ++ tid
  csum <- hashB16 256 chk
  if not $ etalonsCheck tid csum
    then fail $ unpack $ "Etalon csum mismatch, please add to function etalonCheck: '" <> 
      " (\"" <> pack tid <> "\", \"" <> csum <> "\")"
    else do
      tst <- r (etalonPath ++ "M" ++ show n) >>= h bs
      test (pack $ etalonPath ++ "/?" <> tid) (toUpper chk) tst where
        tid = show n ++ "." ++ show bs

test :: Text -> Text -> Text -> IO ()
test msg tst chk = when (chk /= tst) testFail where
  testFail = do
    putStrLn $ "Test '" <> msg <> "' FAILED: '" <> chk <> "' /= '" <> tst <> "'"
    fail $ unpack msg

hashB16 :: Int -> Text -> IO Text
hashB16 bs = hashB16B bs . encodeUtf8

hashB16B :: Int -> ByteString -> IO Text
hashB16B bs = fmap (toUpper . decodeUtf8 . encode) . hashGOST34112012 bs

getCPUTimeNS :: IO Integer
getCPUTimeNS = fmap (flip div cpuTimePrecision) getCPUTime
