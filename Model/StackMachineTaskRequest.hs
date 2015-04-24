module Model.StackMachineTaskRequest where

import Import
import Model.Program

data StackMachineTaskRequest = StackMachineTaskRequest {
    program:: Program
} deriving (Show)

instance FromJSON StackMachineTaskRequest where
    parseJSON (Object o) = StackMachineTaskRequest <$> o .: "program"

    parseJSON _ = mzero