module Common where
import qualified Data.ByteString.Lazy as B
import Zephyr.Utils.Common
import Test.Hspec

printHex :: B.ByteString -> IO ()
printHex = print . encodeHex

buildHex :: Functor f => f B.ByteString -> f String
buildHex b = encodeHex <$> b

testHex :: String -> IO B.ByteString -> IO ()
testHex expected body = do
    bs <- buildHex body
    shouldBe bs expected