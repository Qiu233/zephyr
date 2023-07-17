{-# LANGUAGE TypeApplications #-}
module Zephyr.Encrypt.QQTea (
    tea16KeyFromBytes,
    tea16EmptyKey,
    tea16Encrypt,
    tea16Decrypt,
    qqteaEncrypt,
    qqteaDecrypt,
    TeaKey,
) where
import Data.Word
import qualified Data.ByteString.Lazy as B
import Control.Monad.IO.Class
import Zephyr.Utils.Binary
import qualified Control.Monad.State as State
import Control.Monad (replicateM_)
import Data.Bits ((.|.), (.&.), xor)
import Data.List (mapAccumL)
import Zephyr.Utils.Random

_TEA_DELTA :: Word32
_TEA_DELTA = 0x9E3779B9

type TeaKey = (Word32, Word32, Word32, Word32)
tea16KeyFromBytes :: B.ByteString -> TeaKey
tea16KeyFromBytes = runGet_ (getbe @TeaKey)

tea16EmptyKey :: TeaKey
tea16EmptyKey = tea16KeyFromBytes $ B.replicate 16 0

tea16Encrypt :: TeaKey -> Word64 -> Word64
tea16Encrypt (k0, k1, k2, k3) n =
    let (x,y,_) = State.execState (replicateM_ 16 transfer)
            (fromIntegral $ n .>. 32, fromIntegral n, 0)
        _x = fromIntegral x :: Word64
        _y = fromIntegral y :: Word64 in
    (_x .<. 32) .|. _y
    where
        calc a b c s = foldl xor 0 [a + (b .<. 4) , b + s, c + (b .>. 5)]
        transfer = State.modify $ \(x, y, s) ->
            let s' = s + _TEA_DELTA
                x' = x + calc k0 y  k1 s'
                y' = y + calc k2 x' k3 s' in (x', y', s')

tea16Decrypt :: TeaKey -> Word64 -> Word64
tea16Decrypt key n =
    let (x,y,_) = State.execState (replicateM_ 16 transfer)
            (fromIntegral (n .>. 32), fromIntegral n, _TEA_DELTA .<. 4)
        _x = fromIntegral x :: Word64
        _y = fromIntegral y :: Word64 in
    (_x .<. 32) .|. _y
    where
        (k0, k1, k2, k3) = key
        calc a b c s = foldl xor 0 [a + (b .<. 4) , b + s, c + (b .>. 5)]
        transfer = State.modify $ \(x, y, s) ->
            let y' = y - calc k2 x  k3 s
                x' = x - calc k0 y' k1 s
                s' = s - _TEA_DELTA in (x', y', s')

qqteaEncrypt :: (MonadIO m) => TeaKey -> B.ByteString -> m B.ByteString
qqteaEncrypt key text = do
    randbs <- liftIO $ randBytes fill_count
    --let randbs = B.replicate (fromIntegral fill_count) 220
    let plain_text = runPut $ do
            put8 $ (fromIntegral fill_count- 2) .|. 0xF8
            putbs randbs
            putbs text
            putbs $ B.pack [0,0,0,0, 0,0,0]
    let work_block = runGet_ (getListOfBE @Word64 (div plain_len 8)) plain_text
    pure $ runPut $ putListBE $ compute_work work_block
    where
        len = fromIntegral $ B.length text ::Int
        fill_count = 9 - ((len + 1) `rem` 8)
        plain_len = 1 + fill_count + len + 7
        compute_work = snd . mapAccumL (\(iv1, iv2) x ->
                let iv1' = tea16Encrypt key iv2' `xor` iv2
                    iv2' = x `xor` iv1 in
                ((iv1', iv2'), iv1')) (0,0)

qqteaDecrypt :: TeaKey -> B.ByteString -> B.ByteString
qqteaDecrypt key text =
    let work_block = runGet_ (getListOfBE @Word64 (div len 8)) text
        rst = runPut . putListBE $ compute_work work_block
        begin = fromIntegral (B.head rst .&. 7) + 3
    in B.drop begin $ B.dropEnd 7 rst
    where
        len = fromIntegral $ B.length text ::Int
        compute_work = snd . mapAccumL (\(iv1, iv2) x ->
                let iv2' = tea16Decrypt key $ xor x iv2
                    iv1' = x in
                ((iv1', iv2'), xor iv2' iv1)) (0,0)
