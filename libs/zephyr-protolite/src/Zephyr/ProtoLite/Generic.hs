{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE LambdaCase #-}
module Zephyr.ProtoLite.Generic (
    Optional, Repeated(..), Packed(..), SInt32(..), SInt64(..), Fixed32(..), Fixed64(..),
    ProtoField(..), ProtoBuf(pdef), encode, decode, optOrDef,
    optJust, optNothing, packed, repeated,
    optional, pfield
) where
import GHC.Generics
import Zephyr.ProtoLite.Types
import Data.Kind
import GHC.TypeLits
import Data.Word
import Zephyr.ProtoLite.Encode
import Zephyr.ProtoLite.Decode
import Data.Proxy
import qualified Data.ByteString.Lazy as B
import Data.Int
import GHC.Float
import GHC.IsList (IsList)
import Data.String
import Data.ByteString.Lazy.UTF8 (toString)
import Data.Bits
import GHC.Records
import Prelude hiding (repeat)
import Zephyr.Binary
import Zephyr.Binary.Put
import Zephyr.Binary.Get

type Optional = Maybe
newtype Repeated t = Repeated { repeatedF :: [t] } deriving (Show, Eq, IsList)
newtype Packed t = Packed { packedF :: [t] } deriving (Show, Eq, IsList)
newtype SInt32 = SInt32 Int32 deriving (Show, Eq, Num, Ord, Real, Enum, Integral, Bits)
newtype SInt64 = SInt64 Int64 deriving (Show, Eq, Num, Ord, Real, Enum, Integral, Bits)
newtype Fixed32 t = Fixed32 t deriving (Show, Eq, Num, Ord, Real, Enum, Integral, Bits)
newtype Fixed64 t = Fixed64 t deriving (Show, Eq, Num, Ord, Real, Enum, Integral, Bits)


instance HasField "fixed" (Fixed32 a) a where
    getField (Fixed32 a) = a

instance HasField "fixed" (Fixed64 a) a where
    getField (Fixed64 a) = a

instance (ProtoData a) => HasField "unwrap" (ProtoField (Optional a) n) a where
    getField (ProtoField (Just a)) = a
    getField (ProtoField Nothing) = defpd

instance HasField "unwrap" (ProtoField (Packed a) n) [a] where
    getField (ProtoField (Packed a)) = a

instance HasField "unwrap" (ProtoField (Repeated a) n) [a] where
    getField (ProtoField (Repeated a)) = a

optOrDef :: ProtoData a => Optional a -> a
optOrDef = \case
    Nothing -> defpd
    Just a -> a

optJust :: a -> ProtoField (Optional a) n
optJust = ProtoField . Just

optNothing :: ProtoField (Optional a) n
optNothing = ProtoField Nothing

optional :: ProtoField (Optional a) n -> Maybe a
optional = pv

packed :: [a] -> ProtoField (Packed a) n
packed = ProtoField . Packed

repeated :: [a] -> ProtoField (Repeated a) n
repeated = ProtoField . Repeated

pfield :: t -> ProtoField t n
pfield = ProtoField

encode :: ProtoBuf a => a -> B.ByteString
encode = runPut . pput

decode :: ProtoBuf a => B.ByteString -> a
decode bs = do
    let entries = runGet getMessage bs
    pget entries

newtype ProtoField (t :: Type) (n :: Nat) = ProtoField { pv :: t }
    deriving (Eq, Num, IsString, IsList, Fractional)

class GProtoBuf f where
    gput :: f a -> Put
    gget :: [PMessageEntry] -> f a
    gdef :: f a

class ProtoBuf a where
    pput :: a -> Put
    default pput :: (Generic a, GProtoBuf (Rep a)) => a -> Put
    pput = gput . from

    pget :: [PMessageEntry] -> a
    default pget :: (Generic a, GProtoBuf (Rep a)) => [PMessageEntry] -> a
    pget = to . gget

    pdef :: a
    default pdef :: (Generic a, GProtoBuf (Rep a)) => a
    pdef = to gdef

instance ProtoBuf a => GProtoBuf (K1 i a) where
    gput (K1 a) = pput a
    gget = K1 . pget
    gdef = K1 pdef

instance GProtoBuf p => GProtoBuf (M1 i t p) where
    gput (M1 a) = gput a
    gget = M1 . gget
    gdef = M1 gdef

instance (GProtoBuf f, GProtoBuf g) => GProtoBuf (f :*: g) where
    gput (f :*: g) = gput f >> gput g
    gget = (:*:) <$> gget <*> gget
    gdef = gdef :*: gdef


instance Show t => Show (ProtoField t n) where
    show (ProtoField t) = show t

class ProtoData t where
    putpd :: Word32 -> t -> Put
    getpd :: Word32 -> [PMessageEntry] -> [t]
    defpd :: t

instance (KnownNat n, ProtoData t) => ProtoBuf (ProtoField t n) where
    pput (ProtoField t) = do
        let n = fromIntegral $ natVal (Proxy :: Proxy n)
        putpd n t
    pget vs = do
        let n = fromIntegral $ natVal (Proxy :: Proxy n)
        case getpd n vs of
            [v] -> ProtoField v
            _ -> error $ "no required field " ++ show n
    pdef = ProtoField defpd


instance {-# OVERLAPPING #-} (ProtoData t, KnownNat n) => ProtoBuf (ProtoField (Maybe t) n) where
    pput (ProtoField Nothing) = pure ()
    pput (ProtoField (Just t)) = do
        let n = fromIntegral $ natVal (Proxy :: Proxy n)
        putpd n t
    pget vs = do
        let n = fromIntegral $ natVal (Proxy :: Proxy n)
        case getpd n vs of
            [v] -> ProtoField $ Just v
            _ -> ProtoField Nothing
    pdef = ProtoField Nothing

instance (ProtoBuf a) => (ProtoData a) where
    defpd = pdef
    putpd n v = do
        let bs = runPut $ pput v
        putEntry $ putLenPrefixed n bs
    getpd n vs = do
        let ts = plookupAll n vs
        [pget rs
            | PVLenPrefixed s <- ts, let rs = runGet getMessage s]

instance {-# OVERLAPPING #-} ProtoData String where
    defpd = ""
    putpd n v = putEntry $ putLenPrefixed n (fromString v)
    getpd n vs = do
        let ts = plookupAll n vs
        [toString s | PVLenPrefixed s <- ts]

instance {-# OVERLAPPING #-} ProtoData B.ByteString where
    defpd = B.empty
    putpd n v = putEntry $ putLenPrefixed n v
    getpd n vs = do
        let ts = plookupAll n vs
        [s | PVLenPrefixed s <- ts]

instance {-# OVERLAPPING #-} (ProtoData t, KnownNat n) => ProtoBuf (ProtoField (Repeated t) n) where
    pput (ProtoField (Repeated ts)) = do
        let n = fromIntegral $ natVal (Proxy :: Proxy n)
        mapM_ (putpd n) ts
    pget vs = do
        let n = fromIntegral $ natVal (Proxy :: Proxy n)
        ProtoField $ Repeated $ getpd n vs
    pdef = ProtoField $ Repeated []

class VariantValue t where
    vvdef :: t
    vvto :: t -> PVInt
    vvfrom :: PVInt -> t

instance VariantValue Word32 where{ vvdef = 0; vvto = pvIntDirect'; vvfrom = pvIntDirect }
instance VariantValue Word64 where{ vvdef = 0; vvto = pvIntDirect'; vvfrom = pvIntDirect }
instance VariantValue Int32 where{ vvdef = 0; vvto = pvIntDirect'; vvfrom = pvIntDirect }
instance VariantValue Int64 where{ vvdef = 0; vvto = pvIntDirect'; vvfrom = pvIntDirect }
instance VariantValue SInt32 where{ vvdef = 0; vvto = pvSInt'; vvfrom = pvSInt }
instance VariantValue SInt64 where{ vvdef = 0; vvto = pvSInt'; vvfrom = pvSInt }
instance VariantValue Bool where
    vvdef = False
    vvto = \case
        False -> pvIntDirect' (0 :: Int32)
        True -> pvIntDirect' (1 :: Int32)
    vvfrom = \case
        PVInt 0 -> False
        PVInt 1 -> True
        _ -> error "not a bool"

instance {-# OVERLAPPING #-} ProtoData Word32 where
    defpd = vvdef
    putpd n v = putEntry $ putPVIntIndex n (vvto v)
    getpd n vs = [vvfrom s | PVVariant s <- ts] where ts = plookupAll n vs
instance {-# OVERLAPPING #-} ProtoData Word64 where
    defpd = vvdef
    putpd n v = putEntry $ putPVIntIndex n (vvto v)
    getpd n vs = [vvfrom s | PVVariant s <- ts] where ts = plookupAll n vs
instance {-# OVERLAPPING #-} ProtoData Int32 where
    defpd = vvdef
    putpd n v = putEntry $ putPVIntIndex n (vvto v)
    getpd n vs = [vvfrom s | PVVariant s <- ts] where ts = plookupAll n vs
instance {-# OVERLAPPING #-} ProtoData Int64 where
    defpd = vvdef
    putpd n v = putEntry $ putPVIntIndex n (vvto v)
    getpd n vs = [vvfrom s | PVVariant s <- ts] where ts = plookupAll n vs
instance {-# OVERLAPPING #-} ProtoData SInt32 where
    defpd = vvdef
    putpd n v = putEntry $ putPVIntIndex n (vvto v)
    getpd n vs = [vvfrom s | PVVariant s <- ts] where ts = plookupAll n vs
instance {-# OVERLAPPING #-} ProtoData SInt64 where
    defpd = vvdef
    putpd n v = putEntry $ putPVIntIndex n (vvto v)
    getpd n vs = [vvfrom s | PVVariant s <- ts] where ts = plookupAll n vs
instance {-# OVERLAPPING #-} ProtoData Bool where
    defpd = False
    putpd n v = putEntry $ putPVIntIndex n (if v then pvIntDirect' (1 :: Int32) else pvIntDirect' (0 :: Int32))
    getpd n vs = [(s /= 0) && ((s == 1) || error "not a bool") | PVVariant (PVInt s) <- ts] where ts = plookupAll n vs

getMany :: Get a -> Get [a]
getMany getter = do
    isEmpty >>= \case
        True -> return []
        False -> do
            x <- getter
            xs <- getMany getter
            return (x:xs)

instance {-# OVERLAPPING #-} (KnownNat n, BinPut t, BinGet t) => ProtoBuf (ProtoField (Packed (Fixed64 t)) n) where
    pput (ProtoField (Packed ts)) = do
        let n = fromIntegral $ natVal (Proxy :: Proxy n)
        putEntry $ putLenPrefixed n $ runPut $ mapM_ (putle . (.fixed)) ts
    pget vs = do
        let n = fromIntegral $ natVal (Proxy :: Proxy n)
        ProtoField $ Packed $ concat [runGet (getMany (Fixed64 <$> getle)) s | PVLenPrefixed s <- plookupAll n vs]
    pdef = ProtoField $ Packed []
instance {-# OVERLAPPING #-} (KnownNat n, BinPut t, BinGet t) => ProtoBuf (ProtoField (Packed (Fixed32 t)) n) where
    pput (ProtoField (Packed ts)) = do
        let n = fromIntegral $ natVal (Proxy :: Proxy n)
        putEntry $ putLenPrefixed n $ runPut $ mapM_ (putle . (.fixed)) ts
    pget vs = do
        let n = fromIntegral $ natVal (Proxy :: Proxy n)
        ProtoField $ Packed $ concat [runGet (getMany (Fixed32 <$> getle)) s | PVLenPrefixed s <- plookupAll n vs]
    pdef = ProtoField $ Packed []

instance {-# OVERLAPPING #-} (KnownNat n, VariantValue t) => ProtoBuf (ProtoField (Packed t) n) where
    pput (ProtoField (Packed vs)) = do
        let n = fromIntegral $ natVal (Proxy :: Proxy n)
        putEntry $ putLenPrefixed n $ runPut $ mapM_ (putPVInt . vvto) vs
    pget vs = do
        let n = fromIntegral $ natVal (Proxy :: Proxy n)
        let rs = vvfrom <$> concat [runGet (getMany getPVInt) s | PVLenPrefixed s <- plookupAll n vs]
        ProtoField $ Packed rs
    pdef = ProtoField $ Packed []

class FixedV32 t where
    f32def :: t
    f32to :: t -> Word32
    f32from :: Word32 -> t

class FixedV64 t where
    f64def :: t
    f64to :: t -> Word64
    f64from :: Word64 -> t

instance FixedV32 Float where
    f32def = 0
    f32to = castFloatToWord32
    f32from = castWord32ToFloat

instance FixedV64 Double where
    f64def = 0
    f64to = castDoubleToWord64
    f64from = castWord64ToDouble

instance FixedV32 Word32 where
    f32def = 0
    f32to = id
    f32from = id

instance FixedV64 Word64 where
    f64def = 0
    f64to = id
    f64from = id

instance FixedV32 Int32 where
    f32def = 0
    f32to = fromIntegral
    f32from = fromIntegral

instance FixedV64 Int64 where
    f64def = 0
    f64to = fromIntegral
    f64from = fromIntegral

instance {-# OVERLAPPING #-} (FixedV32 t) => ProtoData (Fixed32 t) where
    defpd = Fixed32 f32def
    putpd n (Fixed32 v) = putEntry $ putFixedU32 n (f32to v)
    getpd n vs = do
        let ts = plookupAll n vs
        [Fixed32 $ f32from s | PVI32 s <- ts]

instance {-# OVERLAPPING #-} (FixedV64 t) => ProtoData (Fixed64 t) where
    defpd = Fixed64 f64def
    putpd n (Fixed64 v) = putEntry $ putFixedU64 n (f64to v)
    getpd n vs = do
        let ts = plookupAll n vs
        [Fixed64 $ f64from s | PVI64 s <- ts]