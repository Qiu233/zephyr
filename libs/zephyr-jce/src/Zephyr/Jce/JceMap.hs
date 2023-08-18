{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Zephyr.Jce.JceMap where
import GHC.IsList
import Data.Maybe

newtype JceMap a b = JceMap [(a, b)]
    deriving (Show, Eq, Semigroup, Monoid)


instance IsList (JceMap a b) where
    type Item (JceMap a b) = (a, b)
    fromList = JceMap
    toList (JceMap m) = m

jlookup :: (Eq a) => a -> JceMap a b -> Maybe b
jlookup k (JceMap m) = lookup k m

jlookupOrEmpty :: (Eq a, Monoid b) => a -> JceMap a b -> b
jlookupOrEmpty k m = fromMaybe mempty $ jlookup k m
