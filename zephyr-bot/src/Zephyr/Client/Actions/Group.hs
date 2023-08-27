{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Zephyr.Client.Actions.Group where
import Zephyr.Client.Types
import Zephyr.Packet.Data.Group
import qualified Data.ByteString.Lazy as B
import Zephyr.Client.Internal
import Zephyr.Core.Entity.Group as EG
import Control.Monad.Except
import Data.Int

fetchGroupList :: Client -> ExceptT String IO [GroupInfo]
fetchGroupList client = rc B.empty
    where
        rc = fix $ \k v -> do
            pkt <- liftIO $ withContext (buildGroupListRequestPacket v) client
            rsp <- sendAndWait pkt client
            (gs, vec) <- decodeGroupListResponse rsp._pkt_body
            if B.null vec
                then pure gs
                else (gs ++) <$> k vec
    

fetchGroupInfo :: Client -> Int64 -> ExceptT String IO GroupInfoDetailed
fetchGroupInfo client group_code = do
    pkt <- liftIO $ withContext (buildGroupInfoRequest group_code) client
    rsp <- sendAndWait pkt client
    decodeGroupInfoResponse rsp._pkt_body