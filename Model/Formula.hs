module Model.Formula where

import Import
import qualified Prelude as P
import qualified Data.Text as T

data Formula = ADD Formula Formula | MUL Formula Formula | SUB Formula Formula | DIV Formula Formula | Value String

instance FromJSON Formula where
    parseJSON (String o) = return $ parseFormula (T.unpack o) [] 0

    parseJSON _ = mzero

instance ToJSON Formula where
    toJSON formula = toJSON $ show formula

instance Show Formula where
    show (Value x) = x
    show (ADD x y) = show x ++ "+" ++ show y
    show (SUB x y) = show x ++ "-" ++ show y
    show (MUL x y) = case (x, y) of
        (ADD _ _, ADD _ _) -> "(" ++ show x ++ ")*(" ++ show y ++ ")"
        (SUB _ _, SUB _ _) -> "(" ++ show x ++ ")*(" ++ show y ++ ")"
        (ADD _ _, SUB _ _) -> "(" ++ show x ++ ")*(" ++ show y ++ ")"
        (SUB _ _, ADD _ _) -> "(" ++ show x ++ ")*(" ++ show y ++ ")"
        (ADD _ _, _) -> "(" ++ show x ++ ")*" ++ show y
        (SUB _ _, _) -> "(" ++ show x ++ ")*" ++ show y
        (_, ADD _ _) -> show x ++ "*(" ++ show y ++ ")"
        (_, SUB _ _) -> show x ++ "*(" ++ show y ++ ")"
        (_, _) -> show x ++ "*" ++ show y
    show (DIV x y) = case (x, y) of
         (ADD _ _, ADD _ _) -> "(" ++ show x ++ ")/(" ++ show y ++ ")"
         (SUB _ _, SUB _ _) -> "(" ++ show x ++ ")/(" ++ show y ++ ")"
         (ADD _ _, SUB _ _) -> "(" ++ show x ++ ")/(" ++ show y ++ ")"
         (SUB _ _, ADD _ _) -> "(" ++ show x ++ ")/(" ++ show y ++ ")"
         (ADD _ _, _) -> "(" ++ show x ++ ")/" ++ show y
         (SUB _ _, _) -> "(" ++ show x ++ ")/" ++ show y
         (_, ADD _ _) -> show x ++ "/(" ++ show y ++ ")"
         (_, SUB _ _) -> show x ++ "/(" ++ show y ++ ")"
         (_, _) -> show x ++ "/" ++ show y

parseFormula (x:xs) sx parCounter
    | x == '*'  && parCounter == 0 = MUL (parseFormula (removePars sx) [] 0) (parseFormula (removePars xs) [] 0)
    | x == '/'  && parCounter == 0 = DIV (parseFormula (removePars sx) [] 0) (parseFormula (removePars xs) [] 0)
    | x == '+'  && parCounter == 0 = ADD (parseFormula (removePars sx) [] 0) (parseFormula (removePars xs) [] 0)
    | x == '-'  && parCounter == 0 = SUB (parseFormula (removePars sx) [] 0) (parseFormula (removePars xs) [] 0)
    | x == '(' = parseFormula xs (sx ++ [x]) (parCounter + 1)
    | x == ')' && parCounter /= 0 = parseFormula xs (sx ++ [x]) (parCounter - 1)
    | null xs = Value (sx ++ [x])
    | otherwise = parseFormula xs (sx ++ [x]) parCounter

removePars string
    | P.head string == '(' && P.last string == ')' = P.tail (P.init string)
    | P.head string == '(' = P.tail string
    | P.last string == ')' = P.init string
    | otherwise = string

findMin (Value x) = x
findMin (MUL x y) =
    if findMin x < findMin y then
        findMin x
    else
        findMin y
findMin (DIV x y) =
    if findMin x < findMin y then
        findMin x
    else
        findMin y
findMin (SUB x y) =
    if findMin x < findMin y then
        findMin x
    else
        findMin y
findMin (ADD x y) =
    if findMin x < findMin y then
        findMin x
    else
        findMin y

sortFormula (Value x) = Value x
sortFormula (MUL x y) =
    if findMin x < findMin y then
        MUL (sortFormula x) (sortFormula y)
    else
        MUL (sortFormula y) (sortFormula x)
sortFormula (DIV x y) =
    if findMin x < findMin y then
        DIV (sortFormula x) (sortFormula y)
    else
        DIV (sortFormula y) (sortFormula x)
sortFormula (SUB x y) =
    if findMin x < findMin y then
        SUB (sortFormula x) (sortFormula y)
    else
        SUB (sortFormula y) (sortFormula x)
sortFormula (ADD x y) =
    if findMin x < findMin y then
        ADD (sortFormula x) (sortFormula y)
    else
        ADD (sortFormula y) (sortFormula x)