module Model.StackMachineRequest where

import Import
import Model.Program
import Model.Stack

data StackMachineRequest = StackMachineRequest {
    stack:: Stack,
    program:: Program,
    programCounter::Integer,
    all::Bool
} deriving (Show)

instance FromJSON StackMachineRequest where
    parseJSON (Object o) = StackMachineRequest <$> o .: "stack" <*> o .: "program" <*> o .: "programCounter" <*> o .: "all"

    parseJSON _ = mzero