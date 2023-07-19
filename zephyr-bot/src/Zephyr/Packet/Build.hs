module Zephyr.Packet.Build (
    buildOicqRequestPacket
) where
import qualified Data.ByteString.Lazy as B
import Data.Word
import Zephyr.Utils.Binary
import Control.Monad
import Zephyr.Core.Context
import Zephyr.Core.Codec
import Control.Lens ((^.))
import Control.Monad.IO.Class (MonadIO)
import Zephyr.Core.Request (Request)
import Zephyr.Core.Transport

data TLV = TLV {
    _tlv_tag :: Word16,
    _tlv_data :: [B.ByteString]
}
marshalTLVs :: TLV -> B.ByteString
marshalTLVs (TLV tag_ data_) = do
    runPut $ do
        put16be tag_
        put16be $ fromIntegral $ length data_
        forM_ data_ putbs


buildOicqRequestPacket :: MonadIO m => Codec -> Word64 -> Word16 -> TLV -> m B.ByteString
buildOicqRequestPacket codec_ uin_ command_ tlvs_ = do
    marshal codec_ msg
    where msg = Message (fromIntegral uin_) command_ EM_ECDH (marshalTLVs  tlvs_)

packRequest :: MonadIO m => Transport -> Request -> m B.ByteString
packRequest tr req = do
    undefined