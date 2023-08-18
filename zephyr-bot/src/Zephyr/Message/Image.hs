{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Zephyr.Message.Image where

data ImageBizType =
    UnknownBizType  
    | CustomFaceImage 
    | HotImage        
    | DouImage        
    | ZhiTuImage      
    | StickerImage    
    | SelfieImage     
    | StickerAdImage  
    | RelatedEmoImage 
    | HotSearchImage
    deriving (Eq, Show)

instance Enum ImageBizType where
    fromEnum UnknownBizType = 0
    fromEnum CustomFaceImage = 1
    fromEnum HotImage = 2
    fromEnum DouImage = 3
    fromEnum ZhiTuImage = 4
    fromEnum StickerImage = 7
    fromEnum SelfieImage = 8
    fromEnum StickerAdImage = 9
    fromEnum RelatedEmoImage = 10
    fromEnum HotSearchImage = 13

    toEnum 0 = UnknownBizType
    toEnum 1 = CustomFaceImage
    toEnum 2 = HotImage
    toEnum 3 = DouImage
    toEnum 4 = ZhiTuImage
    toEnum 7 = StickerImage
    toEnum 8 = SelfieImage
    toEnum 9 = StickerAdImage
    toEnum 10 = RelatedEmoImage
    toEnum 13 = HotSearchImage
    toEnum _ = UnknownBizType