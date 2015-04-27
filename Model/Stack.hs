{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Model.Stack where

import Import
import qualified Data.Text as T
import qualified Prelude as P
import Model.Formula

type Stack = [Formula]

partialEq :: Stack -> Stack -> Bool
partialEq [] _ = False
partialEq result target = (result == target) || partialEq (P.init result) target


arrayToStack :: Text -> Stack
arrayToStack list = map (\x -> parseFormula (T.unpack x) [] 0) (T.splitOn "," (T.tail $ T.init list))

instance ToContent Stack where
    toContent stack = toContent $ show stack
instance ToTypedContent Stack where
    toTypedContent = TypedContent "application/json" . toContent