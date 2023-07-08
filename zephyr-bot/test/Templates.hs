module Templates where

import Language.Haskell.TH
import Text.Printf
import Control.Monad
import Data.Functor


tlvTestTemplate :: Int -> Q Exp
tlvTestTemplate _id = do
    let test_id = "test" ++ printf "%x" _id
        to_test = mkName test_id
        func = mkName "Test.Hspec.it"
    return $ AppE
                (AppE
                    (VarE func)
                    (LitE (StringL test_id)))
                (VarE to_test)

tlvTestTemplate' :: [Int] -> Q Exp
tlvTestTemplate' _ids = do
    mapM (tlvTestTemplate >=> (pure . NoBindS)) _ids <&> DoE Nothing