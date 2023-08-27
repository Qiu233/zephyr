{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Zephyr.Jce.Generic where

import GHC.Generics
import GHC.TypeNats
import Zephyr.Jce.Internal
import Data.Proxy
import Data.Kind
import Data.String (IsString(..))
import GHC.IsList (IsList)
import Zephyr.Binary


newtype JceField (t :: Type) (n :: Natural) = JceField { jval :: t }
    deriving (Eq, Num, IsString, IsList)

instance Show t => Show (JceField t n) where
    show (JceField t) = show t


class GJce f where
    gput :: f a -> Put
    gget :: Get (f a)
    gdef :: f a

class Jce a where
    jput :: a -> Put
    default jput :: (Generic a, GJce (Rep a)) => a -> Put
    jput = gput . from

    jget :: Get a
    default jget :: (Generic a, GJce (Rep a)) => Get a
    jget = to <$> gget

    jdef :: a
    default jdef :: (Generic a, GJce (Rep a)) => a
    jdef = to gdef

instance Jce a => GJce (K1 i a) where
    gput (K1 a) = jput a
    gget = K1 <$> jget
    gdef = K1 jdef

instance GJce p => GJce (M1 i t p) where
    gput (M1 a) = gput a
    gget = M1 <$> gget
    gdef = M1 gdef


instance (GJce f, GJce g) => GJce (f :*: g) where
    gput (f :*: g) = gput f >> gput g
    gget = (:*:) <$> gget <*> gget
    gdef = gdef :*: gdef


instance (KnownNat n, JceData t) => Jce (JceField t n) where
    jput (JceField t) = gjput (fromIntegral . natVal $ (Proxy :: Proxy n)) t
    jget = JceField <$> gjget (fromIntegral . natVal $ (Proxy :: Proxy n))
    jdef = JceField gjdef

instance Jce a => JceData a where
    gjdef = jdef
    gjput n v = do
        putHead 10 n
        jput v
        putHead 11 n
    gjget n = do
        type_ <- skipTo n
        case type_ of
            10 -> do
                v <- jget
                skipToStructEnd
                pure v
            _ -> ueError n type_