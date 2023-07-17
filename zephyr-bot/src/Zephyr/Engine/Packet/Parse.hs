{-# LANGUAGE FlexibleContexts #-}
module Zephyr.Engine.Packet.Parse where
import Zephyr.Engine.Context
import qualified Data.ByteString.Lazy as B
import Control.Lens hiding (Context)
import qualified Zephyr.Core.Signature as Sig
import Data.Word
import Zephyr.Encrypt.QQTea
import Zephyr.Utils.Binary

import qualified Codec.Compression.Zlib as ZLib
import Zephyr.Utils.Common (utf8FromBytes)
import Control.Monad
import Control.Monad.IO.Class (MonadIO)

data SSO = SSO {
    _seqid :: Word32,
    _cmd :: String,
    _payload :: B.ByteString
}

parseSSO :: MonadIO m => B.ByteString -> m SSO
parseSSO buf = do
    let (head_len, seq__, cmd_offset) = flip runGet_ buf $ do
            head_len_ <- get32be
            seq_ <- get32be
            ret_code_ <- get32be
            when (ret_code_ /= 0) $ do error "ret_code_ /= 0"
            offset_ <- (+12) <$> get32be
            pure (head_len_, seq_, offset_)
    let o1 = fromIntegral cmd_offset
    let (cmd, flag) = flip runGet_ (B.drop o1 buf) $ do
            cmd_len_ <- get32be
            cmd_ <- utf8FromBytes <$> getbs (fromIntegral $ cmd_len_ - 4)
            ss_len <- get32be
            _ <- getbs (fromIntegral $ ss_len - 4)
            flag_ <- get32be
            pure (cmd_, flag_)
    let payload_ = case flag of
            0 -> B.drop (fromIntegral $ head_len + 4) buf
            1 -> ZLib.decompress $ B.drop (fromIntegral $ head_len + 4) buf
            8 -> B.drop (fromIntegral head_len) buf
            _ -> error "unknown compressed flag: "
    pure $ SSO seq__ cmd payload_

parsePacket :: MonadIO m => Context -> B.ByteString -> m SSO
parsePacket ctx pkt = do
    let (flag_, encrypted_) = flip runGet_ pkt $ do
            _ <- get32be
            flag <- get8
            _ <- get8
            n <- (+6) <$> get32be
            pure (flag, B.drop (fromIntegral n) pkt)
    decrypted_ <- case flag_ of
            0 -> pure encrypted_
            1 -> do
                let d2key_ = view (signature . Sig.d2key) ctx
                pure $ qqteaDecrypt (tea16KeyFromBytes d2key_) encrypted_
            2 -> do
                pure $ qqteaDecrypt tea16EmptyKey encrypted_
            _ -> error "Unknown flag"
    parseSSO decrypted_
