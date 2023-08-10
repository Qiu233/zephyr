{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Zephyr.Client.Works.Group where
import Zephyr.Client.Types
import Zephyr.Packet.Data.Group
import qualified Data.ByteString.Lazy as B
import Zephyr.Client.Internal
import Zephyr.Core.Entity.Group as EG
import Control.Monad.Except

getGroupList :: Client -> ExceptT String IO [GroupInfo]
getGroupList client = do
    let rc = fix $ \k v -> do
            pkt <- liftIO $ withContext (buildGroupListRequestPacket v) client
            rsp <- sendAndWait pkt client
            (gs, vec) <- decodeGroupListResponse rsp._pkt_body
            if B.null vec
                then pure gs
                else (gs ++) <$> k vec
    rc B.empty