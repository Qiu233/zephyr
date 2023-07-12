{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
module Zephyr.Encrypt.ECDH (
    EncryptECDH(..),
    generateKey,
    generateDefaultKey,
    shared_key,
    public_key,
) where

import qualified Data.ByteString.Lazy as B
import Data.Word
import qualified Crypto.ECC as ECC
import qualified Foreign
import qualified Data.ByteArray as BA
import qualified Data.ByteString
import Data.Proxy
import qualified Crypto.Error as CryError
import Data.Maybe
import Text.Printf
import Data.Functor
import Zephyr.Utils.Common
import Zephyr.Utils.Codec
import Control.Lens

data EncryptECDH = EncryptECDH {
    _shared_key :: B.ByteString,
    _public_key :: B.ByteString
}
$(makeLenses ''EncryptECDH)

instance Show EncryptECDH where
    show (EncryptECDH {..}) =
        printf "EncryptECDH { shared_key = %s, public_key = %s}"
            (encodeHex _shared_key) (encodeHex _public_key)


generateKey :: String -> IO EncryptECDH
generateKey s_pub_key = do
    ECC.KeyPair pub priv <- ECC.curveGenerateKeyPair curve_prox
    let shared = unwrapCryError "failed to generate shared key" $ ECC.ecdh curve_prox priv qqPubKey
    shared_x <- BA.withByteArray shared (Foreign.peekArray @Word8 16) <&> md5Lazy . B.pack
    pure $ EncryptECDH {
        _shared_key = shared_x,
        _public_key = Data.ByteString.fromStrict $ ECC.encodePoint curve_prox pub
    }
    where
        curve_prox = Proxy :: Proxy ECC.Curve_P256R1
        qqPubKey = unwrapCryError "failed to decode QQ public key" $
                    ECC.decodePoint curve_prox $
                    B.toStrict (fromMaybe (error "invalid key") $ decodeHex s_pub_key)
        unwrapCryError msg e = case e of
                                CryError.CryptoPassed v -> v
                                CryError.CryptoFailed _ -> error msg


generateDefaultKey :: IO EncryptECDH
generateDefaultKey = generateKey "04EBCA94D733E399B2DB96EACDD3F69A8BB0F74224E2B44E3357812211D2E62EFBC91BB553098E25E33A799ADC7F76FEB208DA7C6522CDB0719A305180CC54A82E"
