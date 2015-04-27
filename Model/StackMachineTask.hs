{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Model.StackMachineTask where

import Import
import qualified Data.Text as T
import Model.Stack

instance ToJSON (Entity StackMachineTask) where
    toJSON (Entity tid t) = object
        [ "id"      .= (String $ toPathPiece tid)
        , "initialStack"   .= arrayToStack (stackMachineTaskInitialStack t)
        , "targetStack"    .= arrayToStack (stackMachineTaskTargetStack t)
        , "name"   .= stackMachineTaskName t
        , "desc"   .= stackMachineTaskDesc t
        ]

instance FromJSON StackMachineTask where

    parseJSON (Object o) = do
        initialStack <- o .: "initialStack"
        targetStack <- o .: "targetStack"
        StackMachineTask
            (T.pack (show (initialStack :: Stack)))
            (T.pack (show (targetStack :: Stack)))
            <$> o .: "name"
            <*> o .: "desc"

    parseJSON _ = mzero