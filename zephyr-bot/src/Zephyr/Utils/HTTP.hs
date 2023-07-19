{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Zephyr.Utils.HTTP where
import Network.HTTP.Client
import qualified Data.ByteString.Lazy as B
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import Network.HTTP.Client.TLS

httpPostJSON :: Aeson.ToJSON p => String -> p -> IO (Response B.ByteString)
httpPostJSON url body = do
    initialRequest <- parseRequest url
    let request = initialRequest {
        method = "POST",
        requestBody = RequestBodyLBS $ Aeson.encodePretty body,
        requestHeaders = [("Content-Type", "application/json")]
        }
    manager <- newManager tlsManagerSettings
    httpLbs request manager

httpPostJSON_ :: Aeson.ToJSON p => String -> p -> IO B.ByteString
httpPostJSON_ url body = do
    resp <- httpPostJSON url body
    pure $ responseBody resp

httpGET :: String -> IO (Response B.ByteString)
httpGET url = do
    initialRequest <- parseRequest url
    let request = initialRequest {
        method = "GET"
        }
    manager <- newManager tlsManagerSettings
    httpLbs request manager

httpGET_ :: String -> IO B.ByteString
httpGET_ url = do
    resp <- httpGET url
    pure $ responseBody resp