{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Model.StackMachineRating where

import Import

instance FromJSON StackMachineRating where

    parseJSON (Object o) =
        StackMachineRating
            <$> o .: "helpful"
            <*> o .: "understandable"
            <*> o .: "improvements"
            <*> o .: "liked"
            <*> o .: "bugs"

    parseJSON _ = mzero