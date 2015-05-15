module Model.Formula (Formula(..), parseFormula, expandFormula) where

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
    show (SUB x (ADD y z)) = show x ++ "-(" ++ show (ADD y z) ++ ")"
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
         (ADD _ _, Value _) -> "(" ++ show x ++ ")/" ++ show y
         (ADD _ _, _) -> "(" ++ show x ++ ")/(" ++ show y ++ ")"
         (SUB _ _, Value _) -> "(" ++ show x ++ ")/" ++ show y
         (SUB _ _, _) -> "(" ++ show x ++ ")/(" ++ show y ++ ")"
         (_, Value _) -> show x ++ "/" ++ show y
         (_, _) -> show x ++ "/(" ++ show y ++ ")"

instance Eq Formula where
  Value a        == Value b =  a == b
  ADD a1 a2 == ADD b1 b2    =  a1 == b1 && a2 == b2 || a2 == b1 && a1 == b2
  MUL a1 a2 == MUL b1 b2    =  a1 == b1 && a2 == b2 || a2 == b1 && a1 == b2
  DIV a1 a2 == DIV b1 b2    =  a1 == b1 && a2 == b2
  SUB a1 a2 == SUB b1 b2    =  a1 == b1 && a2 == b2
  _              == _               =  False

parseFormula :: String -> String -> Int -> Formula
parseFormula [] _ _ = Value "0"
parseFormula (x:xs) sx parCounter
    | x == '*'  && parCounter == 0 =
        let term = findTerm xs [] 0 in
            if length term == length xs then
                    MUL (parseFormula (removePars sx) [] 0) (parseFormula (removePars xs) [] 0)
            else
                    parseFormula xs (sx ++ [x]) parCounter
    | x == '/'  && parCounter == 0 =
        let term = findTerm xs [] 0 in
            if length term == length xs then
                    DIV (parseFormula (removePars sx) [] 0) (parseFormula (removePars xs) [] 0)
            else
                    parseFormula xs (sx ++ [x]) parCounter
    | x == '+'  && parCounter == 0 = ADD (parseFormula (removePars sx) [] 0) (parseFormula (removePars xs) [] 0)
    | x == '-' && parCounter == 0 && not (null sx) =
        let term = findTerm xs [] 0 in
            if length term == length xs || isDIV (parseFormula xs [] 0) || isMUL (parseFormula xs [] 0) then
                    SUB (parseFormula (removePars sx) [] 0) (parseFormula (removePars xs) [] 0)
            else
                    parseFormula xs (sx ++ [x]) parCounter
    | x == '(' = parseFormula xs (sx ++ [x]) (parCounter + 1)
    | x == ')' && parCounter == 1 && null xs = parseFormula (P.tail sx) [] 0
    | x == ')' && parCounter /= 0 = parseFormula xs (sx ++ [x]) (parCounter - 1)
    | null xs = Value (sx ++ [x])
    | otherwise = parseFormula xs (sx ++ [x]) parCounter

removePars :: String -> String
removePars string
    | P.head string == '(' && P.last string == ')' = P.tail (P.init string)
    | P.head string == '(' = P.tail string
    | P.last string == ')' = P.init string
    | otherwise = string

findTerm :: String -> String -> Int -> String
findTerm (x:xs) sx parCounter
    | x == '*'  && parCounter == 0 = sx
    | x == '/'  && parCounter == 0 = sx
    | x == '+'  && parCounter == 0 = sx
    | x == '-'  && parCounter == 0 && not (null sx) = sx
    | x == '(' = findTerm xs (sx ++ [x]) (parCounter + 1)
    | x == ')' && parCounter == 1 && null xs = sx ++ [x]
    | x == ')' && parCounter /= 0 = findTerm xs (sx ++ [x]) (parCounter - 1)
    | null xs = sx ++ [x]
    | otherwise = findTerm xs (sx ++ [x]) parCounter
findTerm [] _ _ = error "Formel ist fehlerhaft"

expandFormula :: Formula -> Formula
expandFormula (MUL (ADD x y) z) = expandFormula $ ADD (expandFormula (MUL z x)) (expandFormula (MUL z y))
expandFormula (MUL z (ADD x y)) = expandFormula $ ADD (expandFormula (MUL z x)) (expandFormula (MUL z y))
expandFormula (MUL (SUB x y) z) = expandFormula $ SUB (expandFormula (MUL z x)) (expandFormula (MUL z y))
expandFormula (MUL z (SUB x y)) = expandFormula $ SUB (expandFormula (MUL z x)) (expandFormula (MUL z y))
expandFormula (MUL x y) = MUL (expandFormula x) (expandFormula y)

expandFormula (DIV (ADD x y) z) = expandFormula $ ADD (expandFormula (DIV x z)) (expandFormula (DIV y z))
expandFormula (DIV (SUB x y) z) = expandFormula $ SUB (expandFormula (DIV x z)) (expandFormula (DIV y z))
expandFormula (DIV x (DIV y z)) = expandFormula $ MUL (expandFormula x) (expandFormula (DIV z y))
expandFormula (DIV x y) = DIV (expandFormula x) (expandFormula y)

expandFormula (SUB x (ADD y z)) = expandFormula $ SUB (expandFormula x) (expandFormula (SUB y z))
expandFormula (SUB x y) = SUB (expandFormula x) (expandFormula y)
expandFormula (ADD x y) = ADD (expandFormula x) (expandFormula y)
expandFormula (Value x) = Value x

isMUL :: Formula -> Bool
isMUL (MUL _ _) = True
isMUL _ = False

isDIV :: Formula -> Bool
isDIV (DIV _ _) = True
isDIV _ = False