{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
module Zephyr.Packet.Data.Group where
import Zephyr.Core.Request (Request)
import qualified Data.ByteString.Lazy as B
import Zephyr.Core.QQContext
import Control.Lens
import Zephyr.Utils.Jce
import Zephyr.Packet.Jce.TroopListRequest as TLR
import Zephyr.Packet.Jce.RequestDataVersion3
import Zephyr.Utils.Jce.JceMap
import Zephyr.Packet.Build
import Zephyr.Packet.Jce.RequestPacket
import Zephyr.Entity.Group
import Control.Monad.Except
import Zephyr.Utils.Binary
import Zephyr.Packet.Jce.TroopNumber
import Data.Maybe
import Zephyr.Utils.Jce.Internal


buildGroupListRequestPacket :: B.ByteString -> ContextRM Request
buildGroupListRequestPacket vecCookie = do
    uin_ <- fromIntegral <$> view uin
    let req = jdef {
            TLR._uin = uin_,
            _get_msf_msg_flag = 1,
            _cookies = JceField vecCookie,
            _group_info = JceField [],
            _group_flag_ext = 1,
            _version = 7,
            _company_id = 0,
            _version_num = 1,
            _get_long_group_name = 1
    }
    let buf = RequestDataVersion3 $ JceField $
            JceMap [("GetTroopListReqV2Simplify", buildUniRequestData $ jceMarshal req)]
    seq_ <- nextPacketSeq
    let pkt = jdef {
            _i_version = 3,
            _c_packet_type = 0x00,
            _i_request_id = JceField $ fromIntegral seq_,
            _s_servant_name = "mqq.IMService.FriendListServiceServantObj",
            _s_func_name = "GetTroopListReqV2Simplify",
            _s_buffer = JceField $ jceMarshal buf,
            _context = JceField [],
            _status = JceField []
    } :: RequestPacket
    uniPackRequest "friendlist.GetTroopListReqV2" $ jceMarshal pkt

decodeGroupListResponse :: B.ByteString -> ExceptT String IO ([GroupInfo], B.ByteString)
decodeGroupListResponse bs = do
    let request_ = jceUnmarshal bs :: RequestPacket
    let data_ = jceUnmarshal $ request_._s_buffer.jval :: RequestDataVersion3
    let s = B.drop 1 $ fromMaybe B.empty $ jlookup "GetTroopListRespV2" data_._map.jval
    let (vecCookie, groups) = flip runGet s $ do
            a <- getJBytes 4
            b <- gjget 5 :: Get [TroopNumber]
            pure (a, b)
    let groups_ = flip fmap groups $ \g -> do
            let _uin = g._group_uin.jval
                _code = g._group_code.jval
                _name = g._group_name.jval
                _owner_uin = g._group_owner_uin.jval
                _member_count = fromIntegral $ g._member_num.jval
                _max_member_count = fromIntegral $ g._max_group_member_num.jval
            GroupInfo { .. }
    pure (groups_, vecCookie)