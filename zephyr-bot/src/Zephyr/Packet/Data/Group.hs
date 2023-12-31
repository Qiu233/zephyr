{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
module Zephyr.Packet.Data.Group where
import Zephyr.Core.Request (Request)
import qualified Data.ByteString.Lazy as B
import Zephyr.Core.QQContext
import Zephyr.Core.Transport
import Zephyr.Core.AppVersion
import Control.Lens hiding (repeated)
import Zephyr.Jce
import Zephyr.Jce.JceMap
import Zephyr.Packet.Build
import Zephyr.Core.Entity.Group
import Control.Monad.Except
import Zephyr.Binary
import Data.Maybe
import Zephyr.Jce.Internal
import Zephyr.PB.OIDB
import Zephyr.PB.OIDB.OIDB0x88D
import Zephyr.ProtoLite
import Control.Monad.Trans.Except
import Text.Printf (printf)
import Data.Int
import Control.Monad.Reader (asks)
import Zephyr.Packet.JceStructs


buildGroupListRequestPacket :: B.ByteString -> ContextRM Request
buildGroupListRequestPacket vecCookie = do
    uin_ <- fromIntegral <$> view uin
    let req = jdef {
            _uin = uin_,
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


buildGroupInfoRequest :: Int64 -> ContextRM Request
buildGroupInfoRequest code = do
    av <- asks (._transport._app_version)
    let body = D88DReqBody {
            _app_id = optJust $ av._sub_id,
            _pc_client_version = optJust 0,
            _req_group_info = repeated [pdef {
                _group_code = optJust $ fromIntegral code,
                _st_group_info = optJust $ pdef {
                    _group_owner = optJust 0,
                    _group_uin = optJust 0,
                    _group_create_time = optJust 0,
                    _group_flag = optJust 0,
                    _group_member_max_num = optJust 0,
                    _group_member_num = optJust 0,
                    _group_option = optJust 0,
                    _group_level = optJust 0,
                    _group_face = optJust 0,
                    _group_name = optJust "",
                    _group_memo = optJust B.empty,
                    _group_finger_memo = optJust B.empty,
                    _group_last_msg_time = optJust 0,
                    _group_cur_msg_seq = optJust 0,
                    _group_question = optJust B.empty,
                    _group_answer = optJust B.empty,
                    _group_grade = optJust 0,
                    _active_member_num = optJust 0,
                    _head_portrait_seq = optJust 0,
                    --_msg_head_portrait = optJust pdef
                    --_st_group_ex_info
                    _group_sec_level = optJust 0,
                    _cmduin_privilege = optJust 0,
                    _no_finger_open_flag = optJust 0,
                    _no_code_finger_open_flag = optJust 0
                }
            }]
    }
    payload <- packOIDBPackage 2189 0 body
    uniPackRequest "OidbSvc.0x88d_0" payload

decodeGroupInfoResponse :: B.ByteString -> ExceptT String IO GroupInfoDetailed
decodeGroupInfoResponse bs = do
    let rspO = decode bs :: OIDBSSOPkg D88DRspBody
    let rst_ = rspO._result.unwrap
    when (rst_ /= 0) $ do
        throwE $ printf "oidb result unsuccessful: %d msg: %s"
                rspO._result.unwrap rspO._error_msg.unwrap
    let rspM = optional rspO._body
    when (isNothing rspM) $ do
        throwE "oidb result unsuccessful: body is empty"
    let rsp = optOrDef rspM
    let infos = rsp._rsp_group_info.pv.repeatedF
    when (null infos) $ do
        throwE rsp._str_error_info.unwrap
    let info = head infos
    let i = info._group_info.unwrap

    pure $ GroupInfoDetailed {
        _create_time = i._group_create_time.unwrap,
        _group_level = i._group_level.unwrap,
        _last_msg_seq = fromIntegral $ i._group_cur_msg_seq.unwrap,
        _basic_info = GroupInfo {
            _uin = fromIntegral $ i._group_uin.unwrap,
            _code = fromIntegral $ info._group_code.unwrap,
            _name = i._group_name.unwrap,
            _owner_uin = fromIntegral $ i._group_owner.unwrap,
            _member_count = fromIntegral $ i._group_member_num.unwrap,
            _max_member_count = fromIntegral $ i._group_member_max_num.unwrap
        }
    }