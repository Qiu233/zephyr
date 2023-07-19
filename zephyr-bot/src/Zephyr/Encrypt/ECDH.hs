{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Zephyr.Encrypt.ECDH (
    EncryptECDH(..),
    svr_public_key_ver, shared_key, public_key,
    generateDefaultKey, fetchPubKey
) where

import qualified Data.ByteString.Lazy as B
import Data.Word
import qualified Crypto.ECC as ECC
import qualified Foreign
import qualified Data.ByteArray as BA
import qualified Data.ByteString
import Data.Proxy
import qualified Crypto.Error as CryError
import Text.Printf
import Data.Functor
import Zephyr.Utils.Common
import Zephyr.Utils.Codec
import Control.Lens
import qualified Data.Aeson as Aeson
import Control.Monad.Trans.Maybe
import Zephyr.Utils.HTTP
import Data.Aeson ((.:))
import Control.Monad.Trans (lift)
import Zephyr.Utils.MTL

data EncryptECDH = EncryptECDH {
    _svr_public_key_ver :: Word16,
    _shared_key :: B.ByteString,
    _public_key :: B.ByteString
} deriving Eq
$(makeLenses ''EncryptECDH)

instance Show EncryptECDH where
    show (EncryptECDH {..}) =
        printf "EncryptECDH { shared_key = %s, public_key = %s}"
            (encodeHex _shared_key) (encodeHex _public_key)

initKeys :: B.ByteString -> IO (B.ByteString, B.ByteString)
initKeys pub = do
    ECC.KeyPair pub_ priv <- ECC.curveGenerateKeyPair curve_prox
    let shared = unwrapCryError "failed to generate shared key" $ ECC.ecdh curve_prox priv qqPubKey
    shared_x <- BA.withByteArray shared (Foreign.peekArray @Word8 16) <&> md5Lazy . B.pack
    pure (shared_x, Data.ByteString.fromStrict $ ECC.encodePoint curve_prox pub_)
    where
        curve_prox = Proxy :: Proxy ECC.Curve_P256R1
        qqPubKey = unwrapCryError "failed to decode QQ public key" $
                    ECC.decodePoint curve_prox $
                    B.toStrict pub
        unwrapCryError msg e = case e of
                                CryError.CryptoPassed v -> v
                                CryError.CryptoFailed _ -> error msg



generateDefaultKey :: IO EncryptECDH
generateDefaultKey = do
    (shared, pub) <- initKeys $ decodeHex_
        "04EBCA94D733E399B2DB96EACDD3F69A8BB0F74224E2B44E3357812211D2E62EFBC91BB553098E25E33A799ADC7F76FEB208DA7C6522CDB0719A305180CC54A82E"
    pure EncryptECDH { _svr_public_key_ver = 1, _shared_key = shared, _public_key = pub }

data PubKeyMeta = PubKeyMeta {
    _pub_key_ver :: Word16,
    _pub_key :: String,
    _pub_key_sign :: String
}
$(makeLenses ''PubKeyMeta)
instance Aeson.FromJSON PubKeyMeta where
    parseJSON = Aeson.withObject "PubKeyMeta" $ \v -> PubKeyMeta
        <$> v .: "KeyVer"
        <*> v .: "PubKey"
        <*> v .: "PubKeySign"

data PubKeyResp = PubKeyResp {
    _query_span :: Integer,
    _body :: PubKeyMeta
}
$(makeLenses ''PubKeyResp)
instance Aeson.FromJSON PubKeyResp where
    parseJSON = Aeson.withObject "PubKeyResp" $ \v -> PubKeyResp
        <$> v .: "QuerySpan"
        <*> v .: "PubKeyMeta"

fetchPubKey :: Word64 -> IO (Maybe EncryptECDH)
fetchPubKey uin = runMaybeT $ do
    resp <- tryMaybe $ httpGET_ ("https://keyrotate.qq.com/rotate_key?cipher_suite_ver=305&uin=" ++ show uin)
    key_ <- hoistMaybe $ Aeson.decode resp :: MaybeT IO PubKeyResp
    let body_ = key_ ^. body
    let _svr_public_key_ver = body_ ^. pub_key_ver
    (_shared_key, _public_key) <- lift $ initKeys $ decodeHex_ $ body_ ^. pub_key
    pure EncryptECDH { .. }