{-# LANGUAGE FlexibleInstances #-}
module Model.Program where

import Import
import qualified Data.Text as T

data Command = PUSH Int | POP | BRANCHZ Text |
  JUMP Text | MARK Text | PUSHK Text | ADD |
  SUBTRACT | MULTIPLY | DIVIDE | PRINT | SLIDE [Int]
  deriving (Show, Eq)

type Program = [Command]

instance FromJSON Command where
    parseJSON (Object o) = do
        command <- o .: "command"
        if command == ("push" :: Text)
            then PUSH <$> o .: "value"
            else if command == ("pop" :: Text)
                then return POP
                else if command == ("branchz" :: Text)
                    then BRANCHZ <$> o .: "value"
                    else if command == ("jump" :: Text)
                        then JUMP <$> o .: "value"
                        else if command == ("pushK" :: Text)
                            then PUSHK <$> o .: "value"
                            else if command == ("+" :: Text)
                                then return ADD
                                else if command == ("-" :: Text)
                                    then return SUBTRACT
                                    else if command == ("*" :: Text)
                                        then return MULTIPLY
                                        else if command == ("/" :: Text)
                                            then return DIVIDE
                                            else if command == ("slide" :: Text)
                                                then SLIDE <$> o .: "value"
                                                else if command == ("print" :: Text)
                                                  then return PRINT
                                                  else return $ MARK $ T.init command

    parseJSON _ = mzero

instance ToJSON (Command) where
    toJSON (command) = object
        [ "command"      .= getCommand command
        ]
getCommand :: Command -> Text
getCommand (PUSH _) = "push"
getCommand (POP) = "pop"
getCommand (BRANCHZ _) = "branchz"
getCommand (JUMP _) = "jump"
getCommand (MARK x) = x
getCommand (PUSHK _) = "pushK"
getCommand (ADD) = "+"
getCommand (SUBTRACT) = "-"
getCommand (MULTIPLY) = "*"
getCommand (DIVIDE) = "/"
getCommand (SLIDE _) = "slide"
getCommand (PRINT) = "print"