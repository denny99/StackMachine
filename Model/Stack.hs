{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Model.Stack where

import Import

type Stack = [Text]

instance ToContent Stack where
    toContent stack = toContent $ show stack
instance ToTypedContent Stack where
    toTypedContent = TypedContent "application/json" . toContent