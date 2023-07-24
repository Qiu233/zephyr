{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Zephyr.Packet.Wrapper where
import Zephyr.Packet.TLV.Prim (EnergySigner, FekitSigner)
import Network.HTTP.Client
import Zephyr.Utils.Common (utf8ToBytes, encodeHex, decodeHex, utf8FromBytes)
import Text.Printf
import Network.HTTP.Client.TLS
import Zephyr.Utils.HTTP
import qualified Data.Aeson as Aeson
import Data.Aeson ((.:))
import qualified Data.ByteString.Lazy as B
import Control.Monad
import Data.Word
import Control.Monad.Except
import Control.Monad.IO.Class
import Zephyr.Core.Context
import Control.Lens
import Zephyr.Core.Transport
import Zephyr.Core.Device.Types
import Data.Int (Int64)
import qualified Debug.Trace as Debug

data EnergyResp m = EnergyResp {
    _code :: Int,
    _msg :: String,
    _data :: m
} deriving (Show)
instance Aeson.FromJSON m => Aeson.FromJSON (EnergyResp m) where
    parseJSON = Aeson.withObject "EnergyResp" $ \v -> EnergyResp
        <$> v .: "code"
        <*> v .: "msg"
        <*> v .: "data"

data SSOPacket = SSOPacket {
    _cmd :: String,
    _body :: String,
    _callback :: Int64
}
instance Aeson.FromJSON SSOPacket where
    parseJSON = Aeson.withObject "SSOPacket" $ \v -> SSOPacket
        <$> v .: "cmd"
        <*> v .: "body"
        <*> v .: "callback"

data SignResp = SignResp {
    _token :: String,
    _extra :: String,
    _sign :: String,
    _o3did :: String,
    _requestCallback :: [SSOPacket]
}
instance Aeson.FromJSON SignResp where
    parseJSON = Aeson.withObject "SignResp" $ \v -> SignResp
        <$> v .: "token"
        <*> v .: "extra"
        <*> v .: "sign"
        <*> v .: "o3did"
        <*> v .: "requestCallback"

wenergy_ :: String -> String -> String -> Word64 -> String -> String -> B.ByteString -> ExceptT String IO B.ByteString
wenergy_ server android_id_ guid_ uin_ id_ appVersion salt = do
    let len = length server
    let server_ = if (server !! (len - 1)) == '/'
            then server
            else server ++ "/"
    let args = printf "custom_energy?uin=%d&android_id=%s&guid=%s&data=%s&salt=%s" uin_ android_id_ guid_ id_ (encodeHex salt)
    rst <- liftIO $ httpGET_ $ server_ ++ args
    when (B.null rst) $ do
        throwError "Sign Server未返回任何数据，请检查网络问题。"
    let resp = Aeson.decode rst :: Maybe (EnergyResp String)
    let resp_ = maybe (Left "返回Json格式有无，可能是因为服务器版本不对应。") Right resp
    EnergyResp code_ msg_ data_ <- liftEither resp_
    when (code_ /= 0 || null data_) $ do
        throwError $ printf "Sign Server返回错误，错误码：%d，错误信息：%s" code_ msg_
    let data__ = decodeHex data_
    liftEither $ maybe (Left "返回数据格式不是标准十六进制串。") Right data__

wenergy :: ContextIOT m => m EnergySigner
wenergy = do
    sign_server_ <- use sign_server
    android_id_ <- use $ transport . device . android_id
    guid_ <- use $ transport . device . guid
    pure $ \uin_ id_ appVersion salt ->
        runExceptT $ wenergy_ sign_server_ android_id_ (show guid_) uin_ id_ appVersion salt

wsign_ :: String -> String -> String -> Word64 -> String -> String -> String -> B.ByteString -> ExceptT String IO (B.ByteString, B.ByteString, B.ByteString)
wsign_ server android_id_ guid_ seq_ uin_ cmd_ qua_ buff_ = do
    let len = length server
    let server_ = if (server !! (len - 1)) == '/'
            then server
            else server ++ "/"
    let args = printf "sign?uin=%s&qua=%s&cmd=%s&seq=%d&buffer=%s" uin_ qua_ cmd_ seq_ (encodeHex buff_)
    let url = server_ ++ args
    resp <- liftIO $ httpGET_ url
    let resp_ = Aeson.decode resp :: Maybe (EnergyResp SignResp)
    EnergyResp code_ msg_ data_ <- maybe (throwError $ "Sign Server返回Json格式有误。\n"++utf8FromBytes resp) pure resp_
    when (code_ /= 0) $ do
        throwError $ printf "Sign Server返回错误，错误码：%d，错误信息：%s" code_ msg_
    let SignResp token_ extra_ sign_ o3did_ requestCallback_ = data_
    let r d = liftEither $ maybe (Left "返回数据格式不是标准十六进制串。") Right (decodeHex d)
    (,,) <$> r sign_ <*> r extra_ <*> r token_

wsign :: ContextIOT m => m FekitSigner
wsign = do
    sign_server_ <- use sign_server
    android_id_ <- use $ transport . device . android_id
    guid_ <- use $ transport . device . guid
    pure $ \seq_ uin_ cmd_ qua_ buff_ ->
        runExceptT $ wsign_ sign_server_ android_id_ (show guid_) seq_ uin_ cmd_ qua_ buff_