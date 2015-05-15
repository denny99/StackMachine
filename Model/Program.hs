{-# LANGUAGE FlexibleInstances #-}
module Model.Program where

import Import
import qualified Prelude as P

data Command = PUSH Int | POP | BRANCHZ String |
  JUMP String | MARK String | PUSHK String | ADD |
  SUBTRACT | MULTIPLY | DIVIDE | PRINT | SLIDE [Int]
  deriving (Show, Eq)

type Program = [Command]

instance FromJSON Command where
    parseJSON (Object o) = do
        command <- o .: "command"
        if command == ("push" :: String)
            then PUSH <$> o .: "value"
            else if command == ("pop" :: String)
                then return POP
                else if command == ("branchz" :: String)
                    then BRANCHZ <$> o .: "value"
                    else if command == ("jump" :: String)
                        then JUMP <$> o .: "value"
                        else if command == ("pushK" :: String)
                            then PUSHK <$> o .: "value"
                            else if command == ("+" :: String)
                                then return ADD
                                else if command == ("-" :: String)
                                    then return SUBTRACT
                                    else if command == ("*" :: String)
                                        then return MULTIPLY
                                        else if command == ("/" :: String)
                                            then return DIVIDE
                                            else if command == ("slide" :: String)
                                                then SLIDE <$> o .: "value"
                                                else if command == ("print" :: String)
                                                    then return PRINT
                                                    else if P.last command == '.'
                                                        then return $ MARK $ P.init command
                                                        else
                                                            error $ "Unbekannter Befehl: " ++ command

    parseJSON _ = mzero

instance ToJSON (Command) where
    toJSON (command) = object
        [ "command"      .= getCommand command
        ]
getCommand :: Command -> String
getCommand (PUSH _) = "push"
getCommand (POP) = "pop"
getCommand (BRANCHZ _) = "branchz"
getCommand (JUMP _) = "jump"
getCommand (MARK x) = x ++ "."
getCommand (PUSHK _) = "pushK"
getCommand (ADD) = "+"
getCommand (SUBTRACT) = "-"
getCommand (MULTIPLY) = "*"
getCommand (DIVIDE) = "/"
getCommand (SLIDE _) = "slide"
getCommand (PRINT) = "print"