{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE RecordWildCards #-}
module Zephyr.Internal.TLV.T544.ASM where


import Data.Word
import Data.Bits
import qualified Data.ByteString.Lazy as B
import qualified Data.List as List
import Control.Monad.State
import Control.Lens
import qualified Crypto.Cipher.RC4 as RC4
import qualified Data.ByteArray as BArr
import Zephyr.Utils.Binary
import Zephyr.Internal.TLV.T544.HardCoded
import Zephyr.Utils.Time (getEpochTimeMS)
import Zephyr.Utils.Codec
import Zephyr.Utils.Random
import System.Random (randomIO)


type Word128 = (Word32, Word32, Word32, Word32)
get128le :: Get Word128
get128le = do
    l0 <- get32le
    l1 <- get32le
    h2 <- get32le
    h3 <- get32le
    pure (l0, l1, h2, h3)

shiftLR :: Bits a => a -> Int -> Int -> a
shiftLR x l r = (x .<. l) .>. r
shiftRL :: Bits a => a -> Int -> Int -> a
shiftRL x r l = (x .>. r) .<. l

type UnknownI4 = (Word32, Word32, Word32, Word32)

ui4bytes :: UnknownI4 -> B.ByteString
ui4bytes (a, b, c, d) = runPut $ do
    put32le a
    put32le b
    put32le c
    put32le d

sub_a :: B.ByteString -> UnknownI4 -> B.ByteString
sub_a a b = do
    let bs = ui4bytes b
    let s = B.pack $ fmap (B.index bs) sq
    B.packZipWith xor a s
    where
        sq = [3, 2, 1, 0, 7, 6, 5, 4, 11, 10, 9, 8, 15, 14, 13, 12]

sub_a' :: MonadState B.ByteString m => UnknownI4 -> m ()
sub_a' b = modify (`sub_a` b)

sub_b :: B.ByteString -> UnknownI4 -> B.ByteString
sub_b a b = do
    let bs = ui4bytes b
    let s = B.pack $ fmap (B.index bs) sq
    B.packZipWith xor a s
    where
        sq = [3, 6, 9, 12, 7, 10, 13, 0, 11, 14, 1, 4, 15, 2, 5, 8]

sub_b' :: MonadState B.ByteString m => UnknownI4 -> m ()
sub_b' b = modify (`sub_b` b)

sub_c :: [[Word8]] -> B.ByteString -> B.ByteString
sub_c a b = do
    let inner t = a !! fromIntegral (div t 16) !! fromIntegral (rem t 16)
    B.map inner (B.take 16 b)

sub_c' :: MonadState B.ByteString m => [[Word8]] -> m ()
sub_c' a = modify (sub_c a)

sub_d :: [Word8] -> B.ByteString -> B.ByteString
sub_d a b =
    B.pack $ fmap (B.index b . fromIntegral) a

sub_d' :: MonadState B.ByteString m => [Word8] -> m ()
sub_d' a = modify (sub_d a)

sub_e :: [[Word8]] -> B.ByteString -> B.ByteString
sub_e di p = B.concat $ [0..3] <&> \i ->
    let j = 4 * i
        a = p `B.index` j
        b = p `B.index` (j + 1)
        c = p `B.index` (j + 2)
        d = p `B.index` (j + 3) in
    B.pack [
        (c `xor` d) `xor` (getAt a 0 `xor` getAt b 1),
        (a `xor` d) `xor` (getAt b 0 `xor` getAt c 1),
        (a `xor` b) `xor` (getAt d 1 `xor` getAt c 0),
        (b `xor` c) `xor` (getAt a 1 `xor` getAt d 0)
    ]
    where
        getAt x y = di !! fromIntegral x !! y

sub_e' :: MonadState B.ByteString m => [[Word8]] -> m ()
sub_e' a = modify (sub_e a)


sub_ab :: [[Word8]] -> Word32 -> Word32
sub_ab a b = do
    let ia_ = b .>. 24
    let ib_ = b .&. 0xff
    let ic_ = b .>. 8 .&. 0xff
    let id_ = b .>. 16 .&. 0xff
    (getAt' ia_ .<. 24) .|. getAt' ib_ .|. (getAt' ic_ .<. 8) .|. (getAt' id_ .<. 16)
    where
        getAt x y = a !! fromIntegral x !! fromIntegral y
        getAt' x = fromIntegral $ getAt (div x 16) (rem x 16) :: Word32

sub_f :: [Word8] -> [Word32] -> [[Word8]] -> [Word32]
sub_f a b c = do
    let (w_0, w_1, w_2, w_3) = runGet_ (
                (,,,) <$> get32be
                    <*> get32be
                    <*> get32be
                    <*> get32be) $ B.pack a

    [w_0, w_1, w_2, w_3] ++ List.unfoldr gen (4::Int, w_0, w_1, w_2, w_3)
    where
        gen (bx, w_0, w_1, w_2, w_3) =
            if bx >= 44 then
                Nothing
            else do
                let ax = w_3
                let ax' = if rem bx 4 == 0
                    then
                        let t = sub_ab c ((ax .<. 8) .|. (ax .>. 24 .&. 0xff))
                            dx = div (bx - 1) 4 in
                        (b !! dx) `xor` t
                    else ax
                let ax'' = ax' `xor` w_0
                Just (ax'', (bx + 1, w_1, w_2, w_3, ax''))

sub_aa :: Word32 -> [[[[Word8]]]] -> B.ByteString -> Word8 -> Word8
sub_aa a b c d = do
    let dx = (al4 .<. 9) + fromIntegral (d .&. 15 .<. 4)
    let di = fromIntegral $ (c `B.index` fromIntegral al4) .>. 4 :: Word32
    let ax = fromIntegral (d .&. (15 .<. 4)) + (al4 .<. 9) + di :: Word32
    let r1 = getAt ax .<. 4
    let dx' = dx + fromIntegral ((c `B.index` fromIntegral al4) .&. 15) + 256
    r1 .|. getAt dx'
    where
        al4 = a .&. 15 :: Word32
        getAt :: (Integral a, Integral b) => a -> b
        getAt x =
            let x' = fromIntegral x in
            fromIntegral $ b !! div x' 512 !! div (rem x' 512) 256 !! div (rem x' 256) 16 !! rem x' 16

data T544State = T544State {
    _s :: [Word32],
    _org :: [Word32],
    _nr :: Word32,
    _p :: Word32
} deriving (Show, Eq)
makeLenses ''T544State

emptyT544State :: T544State
emptyT544State = T544State [] [] 0 0

transformInner :: B.ByteString -> [[Word8]] -> B.ByteString
transformInner a b = do
    B.pack $ zipWith compute [0..] [1,3..41]
    where
        compute si cnt = do
            let asi = fromIntegral $ a `B.index` si :: Word32
            let ai = ((cnt - 1) .&. 31 .<. 4) + (asi .>. 4) :: Word32
            let bi = (cnt .&. 31 .<. 4) + (asi .&. 15) :: Word32
            ((b !! fromIntegral (div ai 16) !! fromIntegral (rem ai 16)) .<. 4) .|. (b !! fromIntegral (div bi 16) !! fromIntegral (rem bi 16))

initState :: B.ByteString -> B.ByteString -> State T544State ()
initState b c = do
    let w1 = runGet_ get128le b
    let w2 = runGet_ get128le (B.drop 16 b)
    let w3 = runGet_ ((,) <$> get32le <*> get32le) c
    let t = [
            1634760805, 857760878, 2036477234, 1797285236,
            w1 ^. _1, w1 ^. _2, w1 ^. _3, w1 ^. _4,
            w2 ^. _1, w2 ^. _2, w2 ^. _3, w2 ^. _4,
            0, 0, 0, 0 ]
    s .= t
    s . ix 14 .= w3 ^. _1
    s . ix 15 .= w3 ^. _2
    org .= t
    nr .= 20
    p .= 0

data RegState = RegState {
    _bx :: Word32,
    _dx :: Word32,
    _cx :: Word32,
    _bp :: Word32,
    _si :: Word32,
    _di :: Word32,
    _r8 :: Word32,
    _r9 :: Word32,
    _r10 :: Word32,
    _r11 :: Word32,
    _r12 :: Word32,
    _r13 :: Word32,
    _r14 :: Word32,
    _r15 :: Word32
}
makeLenses ''RegState

emptyRegState :: RegState
emptyRegState = RegState {
    _bx = 0,
    _dx = 0,
    _cx = 0,
    _bp = 0,
    _si = 0,
    _di = 0,
    _r8 = 0,
    _r9 = 0,
    _r10 = 0,
    _r11 = 0,
    _r12 = 0,
    _r13 = 0,
    _r14 = 0,
    _r15 = 0
}

-- | Analyzed by New Bing, this procedure works like a variant of chacha20 algorithm.
sub_ad' :: State (T544State, RegState) ()
sub_ad' = do
    r10     <<=         10
    r12     <<=         3
    bp      <<=         11
    dx      <<=         4
    r15     <<=         0
    r9      <<=         12
    si      <<=         5
    r11     <<=         8
    r15     +<=         dx
    r14     <<=         1
    r8      <<=         13
    r9      ^<=         r15
    cx      <<=         6
    r13     <<=         2
    r9      <=>         16
    r14     +<=         si
    bx      <<=         9
    di      <<=         14
    r11     +<=         r9
    r8      ^<=         r14
    r13     +<=         cx
    dx      ^<=         r11
    r8      <=>         16
    di      ^<=         r13
    dx      <=>         12
    bx      +<=         r8
    di      <=>         16
    r15     +<=         dx
    si      ^<=         bx
    r10     +<=         di
    r9      ^<=         r15
    si      <=>         12
    cx      ^<=         r10
    r9      <=>         8
    r14     +<=         si
    cx      <=>         12
    r11     +<=         r9
    r8      ^<=         r14
    r13     +<=         cx
    dx      ^<=         r11
    r8      <=>         8
    di      ^<=         r13
    dx      <=>         7
    bx      +<=         r8
    di      <=>         8
    tmp0 <- loadr       dx
    dx      <<=         7
    si      ^<=         bx
    tmp1 <- loadr       bx
    bx      <<==        r10
    r10     <<=         15
    si      <=>         7
    bx      +<=         di
    r12     +<=         dx
    r15     +<=         si
    r10     ^<=         r12
    cx      ^<=         bx
    r10     <=>         16
    cx      <=>         7
    bp      +<=         r10
    r14     +<=         cx
    dx      ^<=         bp
    r9      ^<=         r14
    dx      <=>         12
    r9      <=>         16
    r12     +<=         dx
    r10     ^<=         r12
    r10     <=>         8
    bp      +<=         r10
    r10     ^<=         r15
    r10     <=>         16
    dx      ^<=         bp
    bp      +<=         r9
    bx      +<=         r10
    dx      <=>         7
    cx      ^<=         bp
    si      ^<=         bx
    si      <=>         12
    r15     +<=         si
    r10     ^<=         r15
    r15     =>>         0
    r10     <=>         8
    bx      +<=         r10
    r10     =>>         15
    si      ^<=         bx
    x1 <- loadr         bx
    si      <=>         7
    cx      <=>         12
    r13     +<=         dx
    r8      ^<=         r13
    r14     +<=         cx
    si      =>>         5
    r8      <=>         16
    r9      ^<=         r14
    r14     =>>         1
    r11     +<=         r8
    r9      <=>         8
    dx      ^<=         r11
    bp      +<=         r9
    r9      =>>         12
    dx      <=>         12
    cx      ^<=         bp
    x2 <- loadr         bp
    r13     +<=         dx
    cx      <=>         7
    r8      ^<=         r13
    cx      =>>         6
    r8      <=>         8
    r13     =>>         2
    r11      +<=        r8
    dx      ^<=         r11
    x0 <- loadr         r11
    dx      <=>         7
    dx      =>>         7
    r8      =>>         13
    si      <<=$        tmp0
    cx      <<=$        tmp1
    r12     +<=         si
    di      ^<=         r12
    di      <=>         16
    cx      +<=         di
    si      ^<=         cx
    dx      <<==        si
    dx      <=>         12
    r12     +<=         dx
    di      ^<=         r12
    r12     =>>         3
    di      <=>         8
    cx      +<=         di
    di      =>>         14
    x3 <- loadr         cx
    dx      ^<=         cx
    dx      <=>         7
    dx      =>>         4
    store   8           x0
    store   9           x3
    store   10          x1
    store   11          x2
    where
        (<<=) reg i = do
            v <- zoom _1 $ do
                uses s (!! i)
            zoom _2 $ do
                reg .= v
        (=>>) reg i = do
            v <- zoom _2 $ use reg
            zoom _1 $ do
                s . ix i .= v
        (+<=) = optrs (+)
        (^<=) = optrs xor
        optrs o dst r = do
            zoom _2 $ do
                v <- use r
                dst %= o v
        (<=>) r n = do
            zoom _2 $ do
                r %= \v -> (v .<. n) .|. (v .>. (32 - n))
        (<<==) dst src = do
            zoom _2 $ do
                v <- use src
                dst .= v
        loadr r = zoom _2 $ use r
        (<<=$) reg v = do
            zoom _2 $ do
                reg .= v
        store i v = do
            zoom _1 $ do
                s . ix i .= v


sub_ad :: State T544State ()
sub_ad = do
    t <- get
    let t' = execState sub_ad' (t, emptyRegState)
    put $ t' ^. _1

refresh :: State T544State ()
refresh = do
    cx_ <- use nr <&> (`div` 2)
    replicateM_ (fromIntegral cx_) sub_ad
    o <- use org
    s %= zipWith (+) o

whileM :: Monad m => m Bool -> m a -> m [a]
whileM p_ f = go
    where 
        go = do
            x <- p_
            if x
                then do
                        x'  <- f
                        xs <- go
                        return (return x' `mplus` xs)
                else return mzero
whileM_ :: Monad m => m Bool -> m a -> m ()
whileM_ p_ f = do
    _ <- whileM p_ f
    pure ()

(<&&>) :: Monad m => m Bool -> m Bool -> m Bool
(<&&>) a b = do
    a' <- a
    if a'
        then b
        else pure False

encrypt' :: State (T544State, [Word8], Word32, Word32) ()
encrypt' = do
    _3 .= 0
    _4 <~ fromIntegral . length <$> use _2
    whileM_ (uses _4 (> 0)) $ do
        zoom _1 $ do
            sp <- use p
            when (sp == 0) refresh
        os <- use $ _1 . s
        let sb = B.unpack $ runPut $ mapM_ put32le os
        whileM_ (uses (_1 . p) (/= 64) <&&> uses _4 (/= 0)) $ do
            bp_ <- fromIntegral <$> use _3
            p_ <- fromIntegral <$> use (_1 . p)
            _2 . ix bp_ %= xor (sb !! p_)
            _1 . p += 1
            _3 += 1
            _4 -= 1
        p_ <- use (_1 . p)
        when (p_ >= 64) $ do
            _1 . p .= 0
            _1 . org . ix 12 += 1
            _1 . s <~ use (_1 . org)

encrypt :: [Word8] -> State T544State [Word8]
encrypt data_ = do
    t <- get
    let t' = execState encrypt' (t, data_, 0, 0)
    put $ t' ^. _1
    pure $ t' ^. _2


tencentEncryptionA :: B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString
tencentEncryptionA inputData key data_ = do
    let t544State = emptyT544State
    flip evalState t544State $ do
            initState key data_
            encrypt (B.unpack inputData) <&> B.pack

_tencentEncryptB :: [Word32] -> State B.ByteString ()
_tencentEncryptB p2 = do
    let c = 10
    [0..8] `forM_` \i -> do
        sub_d' tableD
        sub_b' (get4 i)
        sub_c' tableB
        sub_e' tableC
    sub_d' tableD
    sub_b' (get4 $ c-1)
    sub_c' tableB
    sub_a' (get4 c)
    where
        get4 n = (p2 !! (n*4), p2 !! (n*4+1), p2 !! (n*4+2), p2 !! (n*4+3))

tencentEncryptB' :: B.ByteString -> State (B.ByteString, B.ByteString, B.ByteString) ()
tencentEncryptB' m = do
    let w = sub_f tableE tableF tableB
    _2 .= B.replicate 16 0
    _3 .= B.replicate 21 0
    [0..20] `forM_` \i -> do
        when ((i .&. 0xf) == 0) $ do
            _2 <~ use _1
            zoom _2 $ _tencentEncryptB w
            zoom _1 $ do
                let inner j = do
                        ix j += 1
                        v <- gets (`B.index` j)
                        when (j >=0 && v == 0) $ do
                            inner (j-1)
                inner 15
        buf <- use _2
        _3 . ix i .= sub_aa (fromIntegral i) tableA buf (m `B.index` i)

tencentEncryptB :: B.ByteString -> B.ByteString -> B.ByteString
tencentEncryptB c m = do
    let t = execState (tencentEncryptB' m) (c, B.empty, B.empty)
    t ^. _3

tencentCrc32 :: [Word32] -> [Word8] -> Word32
tencentCrc32 a b = do
    let c = foldl (\crc v -> (a!!((fromIntegral crc .&. 0xff) `xor` v)) `xor` (crc .>. 8))
            (complement 0) (fmap fromIntegral b)
    complement c

data SignDeps = SignDeps {
    _timeMS :: Int,
    _r1 :: Word8,
    _r2 :: Word8,
    _b1 :: Word8,
    _b2 :: Word8,
    _nonce :: Word32,
    _addition :: Word8
}

testSignDeps :: SignDeps
testSignDeps = SignDeps 1689306385790 95 87 113 37 2882093107 1


sign :: (MonadIO m) => B.ByteString -> m B.ByteString
sign i = do
    deps <- SignDeps
        <$> (getEpochTimeMS <&> (`rem` 1000000))
        <*> (randPickB keyTable <&> (+50))
        <*> (randPickB keyTable <&> (+50))
        <*> randPickB table2
        <*> randPickB table2
        <*> randomIO
        <*> randRUntil (0, 8) (\x -> (x .&. 1) /= 0)
    pure $ sign' deps i
    where
        randPickB table = do
            t <- randR (0, B.length table - 1)
            pure $ B.index table t

        randRUntil r cond = do
            t <- randR r
            if cond t then pure t else randRUntil r cond

computeKey :: B.ByteString -> B.ByteString
computeKey kt4 = do
    let key = do
            let raw = B.packZipWith xor key2 kt4
            let ys = (B.take 2 . B.drop 2 $ raw) <> B.take 4 key1 <> B.take 2 raw
            (BArr.pack @BArr.ScrubbedBytes) . B.unpack $ ys
    let (_, encKey) = RC4.combine (RC4.initialize key) key
    B.fromStrict $ BArr.convert encKey

sign' :: SignDeps -> B.ByteString -> B.ByteString
sign' SignDeps{..} input_ = do
    runPut $ do
        put8 0x0C
        put8 0x05
        putbs nonce
        putbs encrypted
        put32le 0
        put8 _b1
        put8 _b2
        put8 $ _b1 + (_addition .&. 0xff)
        -- TODO: precedence here is different from oicq but is consistent to cq-http
        -- Since oicq is transpiled from cq-http, I decided to keep this
        put8 $ _b2 + ((9-_addition) .&. 0xff) + 1
    where
        overBS pos src = do
            let vs = B.unpack src
            let len = B.length src
            let ixs = [ix t | t <- [pos..(pos+len-1)]]
            zipWithM_ (.=) ixs vs
        overBy pos gen = do
            overBS pos $ runPut gen
        timeMS = fromIntegral (_timeMS`rem` 1000000)
        tbs = runPut (put32be timeMS)
        mn = runPut $ put64le 0x6EEDCF0DC4675540
        kt4 = runPut $ do
            put8 _r1
            put8 _r2
            put8 $ _r2 + 20
            put8 $ _r2 + 40
        encKey = computeKey kt4
        input = tencentEncryptionA (input_ <> tbs) (B.take 32 $ encKey <> B.replicate 32 0) mn
        hash = md5Lazy input
        ms = (tbs <> mn <> B.replicate 9 0) &~ do
            ix 2 .= 1
            ix 4 .= 1
            overBy 5 $ do
                putbs kt4
                put32be timeMS
                putbs (B.take 8 hash)
        crc = tencentCrc32 tab (B.unpack . B.drop 2 $ ms)
        ms_ = ms &~ do
            ix 0 .= fromIntegral (crc .&. 0xff)
            ix 1 .= fromIntegral (crc .>. 24)
        nonce = runPut $ put32le _nonce
        ms__ = transformInner ms_ transFormTableEncode
        us = tencentEncryptB (B.take 16 $ B.cycle nonce) ms__
        encrypted = transformInner us transformTableDecode