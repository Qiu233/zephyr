{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Zephyr.Packet.Login.Parse where
import qualified Data.ByteString.Lazy as B
import Zephyr.Core.QQContext
import Zephyr.Packet.Login.Types
import Data.Word
import Data.HashMap
import Zephyr.Core.Transport
import Zephyr.Core.Device
import Zephyr.Utils.GUID
import Control.Lens
import Zephyr.Utils.Common
import Zephyr.Utils.Random
import Zephyr.Core.Signature
import Zephyr.Utils.Codec
import Zephyr.Packet.Login.Pow
import Zephyr.Packet.TLV.Decoders
import Zephyr.Packet.Internal
import Zephyr.Utils.Binary
import Data.Proxy
import Zephyr.Utils.Map
import Control.Monad.Except
import Zephyr.Utils.Jce
import Zephyr.Packet.Jce.RequestPacket
import Zephyr.Packet.Jce.RequestDataVersion2
import Zephyr.Utils.Jce.JceMap
import Zephyr.Packet.Jce.SvcRespRegister
import Control.Monad.Trans.Except
import Data.Either
import Data.Maybe (fromMaybe)




decodeLoginResponse :: B.ByteString -> ContextOPM LoginResponse
decodeLoginResponse bs = do
    mp (es !> 0x402) $ \x -> do
        guid_ <- uses (transport . device . guid) guidBytes
        zoom (transport . signature) $ do
            rs <- utf8ToBytes <$> randString 16
            dpwd .= rs
            t402 .= x
            g .= md5Lazy (guid_ <> rs <> x)
    mp (es !> 0x546) $ \x -> do
        y <- calcPow x
        transport . signature . t547 .= y
    case t of
        0 -> do -- success
            mp (es !> 0x403) (transport . signature . rand_seed .=)
            tgtgt_key_ <- use (transport . device . tgtgt_key)
            decodeT119 (es ?> 0x119) tgtgt_key_
            pure LoginSuccess
        2 -> do -- captcha
            transport . signature . t104 .= es ?> 0x104
            if has_ es 0x192 then do
                pure $ SliderNeeded $ utf8FromBytes (es ! 0x192)
            else if has_ es 0x165 then do
                let tmp = es ! 0x165
                let (sign_, data_) = flip runGet tmp $ do
                        slen <- get16be
                        _ <- get16be
                        sign__ <- getbs $ fromIntegral slen
                        data__ <- getRemaining
                        pure (sign__, data__)
                pure $ NeedCaptcha data_ sign_
            else pure $ UnknownLoginResponse t ""
        40 -> do -- frozen
            pure AccountFrozen
        162 -> do -- too many sms
            pure TooManySMSRequest
        204 -> do -- device lock
            transport . signature . t104 .= es ?> 0x104
            transport . signature . rand_seed .= es ?> 0x403
            pure DeviceLockLogin
        t_ -> do
            ps <-
                if t_ == 160 || t_ == 239 then do
                    if has_ es 0x174 then do
                        let t174_ = es ! 0x174
                        transport . signature . t104 .= es ?> 0x104
                        transport . signature . t174 .= t174_
                        transport . signature . rand_seed .= es ?> 0x403
                        let phone_ = flip runGet (es ! 0x178) $ do
                                _ <- getlv
                                utf8FromBytes <$> getlv
                        if has_ es 0x204 then do
                            pure $ Just $ VerificationNeeded
                                (utf8FromBytes $ es ?> 0x17E)
                                (utf8FromBytes $ es ! 0x204)
                                phone_
                        else do
                            pure $ Just $ SMSNeeded (utf8FromBytes $ es ?> 0x17E) phone_
                    else if has_ es 0x17B then do -- second time
                        transport . signature . t104 .= es ?> 0x104
                        pure $ Just $ SMSNeeded "" "" -- ?
                    else if has_ es 0x204 then do
                        pure $ Just $ VerificationNeeded "" (utf8FromBytes $ es ! 0x204) ""
                    else pure Nothing
            else pure Nothing
            case ps of
                Just x -> pure x
                Nothing -> do
                    if has_ es 0x149 then do
                        let r_ = flip runGet (es ! 0x149) $ do
                                _ <- get16be
                                _ <- getlv -- title
                                utf8FromBytes <$> getlv
                        pure $ UnknownLoginResponse t r_
                    else if has_ es 0x146 then do
                        let r_ = flip runGet (es ! 0x146) $ do
                                _ <- get32be
                                _ <- getlv -- title
                                utf8FromBytes <$> getlv
                        pure $ UnknownLoginResponse t r_
                    else pure $ UnknownLoginResponse t ""
    where
        (_, t, _, r) = flip runGet bs $ do
            (,,,) <$> get16be <*> get8 <*> get16be <*> getRemaining
        es = runGet (getTLVEntries (Proxy @Word16)) r :: Map Word16 B.ByteString
        has_ = flip Data.HashMap.member

decodeClientRegisterResponse :: B.ByteString -> ExceptT String ContextRM ()
decodeClientRegisterResponse bs = do
    let request_ = jceUnmarshal bs :: RequestPacket
    let data_ = jceUnmarshal $ jceUnwrap (request_._s_buffer) :: RequestDataVersion2
    let m_ = jceUnwrap $ data_._map
    let svcRspM = jceUnmarshal_ $ B.drop 1 $ fromMaybe B.empty (jlookup "SvcRespRegister" m_ >>= jlookup "QQService.SvcRespRegister") :: Either String SvcRespRegister
    when (isLeft svcRspM) $ do
        throwE $ fromLeft undefined svcRspM
    let svcRsp = fromRight undefined svcRspM
    when (jceUnwrap svcRsp._result /= "" || jceUnwrap svcRsp._reply_code /= 0) $ do
        throwE "reg failed"