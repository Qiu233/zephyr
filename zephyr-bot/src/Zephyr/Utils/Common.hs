{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Zephyr.Utils.Common where
import qualified Data.ByteString.Lazy as B
import Data.Word
import Numeric (readHex)
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import Data.Maybe (fromJust)

slice :: Int -> Int -> [a] -> [a]
slice s e = take (e - s) . drop s

sliceToEnd :: Int -> [a] -> [a]
sliceToEnd s l = slice s (length l) l

decodeHex :: String -> Maybe B.ByteString
decodeHex s =
    let ss = part 2 s in
    let y = concat [ readHex x | x <- ss ] in
    let z = concatMap snd y in
    if null z then
        Just $ B.pack $ fmap fst y -- TODO: efficient? can we build a bytestring incrementally?
    else
        Nothing
    where
        part _ [] = []
        part n xs = take n xs : part n (drop n xs)

decodeHex_ :: String -> UTF8.ByteString
decodeHex_ = fromJust . decodeHex

encodeHex :: B.ByteString -> String
encodeHex =
    concatMap (\x ->
        let y = div x 16
            z = rem x 16 in
        [toc y, toc z]) . B.unpack
    where
        toc :: Word8 -> Char
        toc y = if x < 10 then toEnum @Char $ fromEnum '0' + x
                else toEnum @Char $ fromEnum 'A' + x - 10
                where x = fromIntegral y