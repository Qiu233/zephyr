{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Zephyr.Core.Device.QIMEI (
    Payload,
    genRandomPayloadByDevice,
    requestQImei,
    aesEncrypt,
    aesDecrypt
) where
import qualified Control.Monad.Except as ExceptT
import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import Data.Time
import Crypto.Cipher.AES
import Crypto.Cipher.Types

import qualified Zephyr.Core.Device.Types as Dev
import qualified Crypto.PubKey.RSA as RSA
import qualified OpenSSL.RSA as ORSA
import Data.Word
import qualified Zephyr.Core.ClientApp as CA
import Network.HTTP.Client hiding (host)
import Network.HTTP.Client.TLS
import Control.Lens

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Base64.Lazy as Base64
import qualified Data.ByteString as SB
import Zephyr.Utils.Common
import Data.Maybe (fromJust, fromMaybe)
import Crypto.Error (throwCryptoError)
import Zephyr.Utils.Random
import qualified Data.List as List
import Text.Printf (printf)
import qualified OpenSSL.PEM as PEM
import qualified OpenSSL.EVP.PKey as PKey
import Data.Aeson ((.:))
import Control.Monad.IO.Class
import Zephyr.Utils.Time
import qualified Crypto.PubKey.RSA.PKCS15 as PKCS15
import Zephyr.Utils.Codec (md5OfU8)
import Crypto.Data.Padding



aesEncrypt :: B.ByteString -> B.ByteString -> B.ByteString
aesEncrypt src' key' = do
    let src = pad (PKCS7 16) $ B.toStrict src'
    let key = B.toStrict key'
    let cipherIV = fromJust $ makeIV (SB.take 16 key) :: IV AES128
    let cipher = throwCryptoError $ cipherInit key :: AES128
    Base64.encode $ B.fromStrict $ cbcEncrypt cipher cipherIV src

aesDecrypt :: B.ByteString -> B.ByteString -> B.ByteString
aesDecrypt src' key' = do
    let src = B.toStrict $ Base64.decodeLenient src'
    let key = B.toStrict key'
    let cipherIV = fromJust $ makeIV (SB.take 16 key) :: IV AES128
    let cipher = throwCryptoError $ cipherInit key :: AES128
    let rst = fromJust $ unpad (PKCS7 16) $ cbcDecrypt cipher cipherIV src
    B.fromStrict rst

data PayloadInner = PayloadInner {
    harmony :: String,
    clone :: String,
    containe :: String,
    oz :: String,
    oo :: String,
    kelong :: String,
    uptimes :: String,
    multiUser :: String,
    bod :: String,
    brd :: String,
    dv :: String,
    firstLevel :: String,
    manufact :: String,
    name :: String,
    host :: String,
    kernel :: String
} deriving (Generic, Show)

data Payload = Payload {
    androidId :: String,
    platformId :: Word32,
    appKey :: String,
    appVersion :: String,
    beaconIdSrc :: String,
    brand :: String,
    channelId :: String,
    cid :: String,
    imei :: String,
    imsi :: String,
    mac :: String,
    model :: String,
    networkType :: String,
    oaid :: String,
    osVersion :: String,
    qimei :: String,
    qimei36 :: String,
    sdkVersion :: String,
    audit :: String,
    userId :: String,
    packageId :: String,
    deviceType :: String,
    sdkName :: String,
    reserved :: String
} deriving (Generic, Show)

instance Aeson.ToJSON PayloadInner
instance Aeson.ToJSON Payload

fmtTime :: String -> UTCTime -> String
fmtTime = formatTime defaultTimeLocale

genRandomPayloadByDevice :: CA.ClientApp -> Dev.Device -> IO Payload
genRandomPayloadByDevice ver dev = do
    uptimes <- fmtTime "%F %T" <$> getCurrentTime
    let rs = PayloadInner {
            harmony = "0",
            clone = "0",
            containe = "",
            oz = "UhYmelwouA+V2nPWbOvLTgN2/m8jwGB+yUB5v9tysQg=",
            oo = "Xecjt+9S1+f8Pz2VLSxgpw==",
            kelong = "0",
            uptimes = uptimes,
            multiUser = "0",
            bod = view Dev.board dev,
            brd = view Dev.board dev,
            dv = view Dev.device_name dev,
            firstLevel = "",
            manufact = view Dev.brand dev,
            name = view Dev.model dev,
            host = "se.infra",
            kernel = view Dev.fingerprint dev
            }
    timeMonth <- fmtTime "%Y-%m-01" <$> getCurrentTime
    rand1 <- randR (0, 899999) <&> (+ 100000) :: IO Word32
    rand2 <- randR (0, 899999999) <&> (+ 100000000) :: IO Word32
    beaconId <- concat <$> sequence (flip fmap [1..40::Int] $
            \x ->
                if x `List.elem` [1, 2, 13, 14, 17, 18, 21, 22, 25, 26, 29, 30, 33, 34, 37, 38] then
                    pure $ printf "k%d:%s%d.%d;" x timeMonth rand1 rand2
                else if x == 3 then
                    pure "k3:0000000000000000;" :: IO String
                else if x == 4 then do
                    k4s <- randHexl 16 :: IO String
                    pure $ printf "k4:%s;" k4s
                else do
                    kis <- randR (0, 10000-1) :: IO Int
                    pure $ printf "k%d:%d;" x kis)

    pure $ Payload {
        androidId = view Dev.android_id dev,
        platformId = 1,
        appKey = view CA.app_key ver,
        appVersion = view CA.version ver,
        beaconIdSrc = beaconId,
        brand = view Dev.brand dev,
        channelId = "2017",
        cid = "",
        imei = view Dev.imei dev,
        imsi = "",
        mac = "",
        model = view Dev.model dev,
        networkType = "unknown",
        oaid = "",
        osVersion =
            let Dev.OSVersion _ release _ sdk = view Dev.os_version dev in
                    printf "Android %s,level %d" release sdk,
        qimei = "",
        qimei36 = "",
        sdkVersion = "1.2.13.6",
        audit = "",
        userId = "{}",
        packageId = view CA.id ver,
        deviceType =
            if view CA.display ver == "aPad" then
                "Pad"
            else
                "Phone",
        sdkName = "",
        reserved = utf8FromBytes $ Aeson.encodePretty rs
    }

secret :: String
secret = "ZdJqM15EeO2zWc08"

rsaKey :: String
rsaKey = "-----BEGIN PUBLIC KEY-----\n\
\MIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQDEIxgwoutfwoJxcGQeedgP7FG9\n\
\qaIuS0qzfR8gWkrkTZKM2iWHn2ajQpBRZjMSoSf6+KJGvar2ORhBfpDXyVtZCKpq\n\
\LQ+FLkpncClKVIrBwv6PHyUvuCb0rIarmgDnzkfQAqVufEtR64iazGDKatvJ9y6B\n\
\9NMbHddGSAUmRTCrHQIDAQAB\n\
\-----END PUBLIC KEY-----"


readPublicKey :: String -> IO RSA.PublicKey
readPublicKey str = do
  pem <- PKey.toPublicKey <$> PEM.readPublicKey str :: IO (Maybe ORSA.RSAPubKey)
  case pem of
    Just key -> do
        pure $ RSA.PublicKey {
            RSA.public_size = ORSA.rsaSize key,
            RSA.public_n = ORSA.rsaN key,
            RSA.public_e = ORSA.rsaE key
            }
    _ -> error "Invalid PEM file"


randHexl :: MonadIO m => Int -> m String
randHexl n = do
    randPickn n "0123456789abcdef"

data ReqBody = ReqBody {
    key :: String,
    params :: String,
    time :: Int,
    nonce :: String,
    sign :: String,
    extra :: String
} deriving (Generic, Show)
instance Aeson.ToJSON ReqBody

data ReqResp = ReqResp {
    data_ :: String,
    code :: Integer
} deriving (Generic, Show)
instance Aeson.FromJSON ReqResp where
    parseJSON = Aeson.withObject "Person" $ \v -> ReqResp
        <$> v .: "data"
        <*> v .: "code"

data ReqRespInner = ReqRespInner {
    q16 :: String,
    q36 :: String
} deriving (Generic, Show)
instance Aeson.FromJSON ReqRespInner

postJson :: Aeson.ToJSON p => String -> p -> IO (Response B.ByteString)
postJson url body = do
    initialRequest <- parseRequest url
    let request = initialRequest {
        method = "POST",
        requestBody = RequestBodyLBS $ Aeson.encodePretty body,
        requestHeaders = [("Content-Type", "application/json")]
        }
    manager <- newManager tlsManagerSettings
    httpLbs request manager

requestQImei :: CA.ClientApp -> Dev.Device -> ExceptT.ExceptT String IO (String, String)
requestQImei ver dev = do
    payload <- liftIO $ Aeson.encodePretty <$> genRandomPayloadByDevice ver dev
    cryptKey <- utf8ToBytes <$> randHexl 16
    ts <- liftIO getEpochTimeMS
    nonce <- randHexl 16
    publicKey <- liftIO $ readPublicKey rsaKey
    key <- liftIO (PKCS15.encrypt publicKey (SB.toStrict cryptKey)) >>= \case
                Left err -> ExceptT.throwError $ "Failed to create key" ++ show err
                Right key -> pure $ utf8FromBytes . Base64.encode . B.fromStrict $ key
    let params = utf8FromBytes $ aesEncrypt payload cryptKey
    let body = ReqBody {
            key = key,
            params = params,
            time = ts,
            nonce = nonce,
            sign = encodeHex . B.fromStrict . md5OfU8 $ (key ++ params ++ show ts ++ nonce ++ secret),
            extra = ""
            }
    response <- liftIO $ postJson "https://snowflake.qq.com/ola/android" body
    let resp = Aeson.decode $ responseBody response :: Maybe ReqResp
    resp' <- maybe (ExceptT.throwError $ "Failed to decode response: \n" ++ show response) pure resp
    if code resp' == 0 then do
        let respInner = Aeson.decode $ aesDecrypt (utf8ToBytes $ data_ resp') cryptKey :: Maybe ReqRespInner
        case respInner of
            Just respInner' -> pure (q16 respInner', q36 respInner')
            Nothing -> ExceptT.throwError $ "Bad json format detected: " ++ show resp'
    else
        ExceptT.throwError $ "Code != 0: " ++ show resp'