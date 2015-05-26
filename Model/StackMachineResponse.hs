module Model.StackMachineResponse where

import Import
import Model.Stack

data StackMachineResponse = StackMachineResponse {
    stack:: Stack,
    programCounter::Integer,
    jumpCounter::Integer
} deriving (Show)

instance ToJSON (StackMachineResponse) where
    toJSON (response) = object
        [ "stack"           .= stack response,
          "programCounter"  .= programCounter response,
          "jumpCounter"     .= jumpCounter response
        ]

instance ToContent StackMachineResponse where
    toContent response = toContent $ toJSON response
instance ToTypedContent StackMachineResponse where
    toTypedContent = TypedContent "application/json" . toContent