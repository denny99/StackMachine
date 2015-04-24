module Model.StackMachine where

import Import
import Data.Char(isDigit)
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Maybe as M
import qualified Prelude as P
import Model.Program
import Model.Stack
import qualified Model.StackMachineResponse as Response

--stackMachine operations
executeAll :: Stack -> Program -> Int -> Int -> Response.StackMachineResponse
executeAll stack program programCounter jumpCounter
    | jumpCounter > 1000 = error "Nicht mehr als 1000 Sprünge erlaubt"
    | length program == programCounter = Response.StackMachineResponse stack programCounter jumpCounter
    | otherwise = do
        let response = execute (program P.!! programCounter) stack program programCounter jumpCounter
        executeAll (Response.stack response) program (Response.programCounter response) (Response.jumpCounter response)

execute :: Command -> Stack -> Program -> Int -> Int -> Response.StackMachineResponse
execute (PUSH x) stack _ programCounter jumpCounter =
    if x < length stack then
        Response.StackMachineResponse (stack P.!! x : stack) (programCounter + 1) jumpCounter
    else
        error "Stackadresse existiert nicht"

execute POP [] _ _ _ = error "Stack enthält keine Elemente"
execute POP (_:stack) _ programCounter jumpCounter = Response.StackMachineResponse stack (programCounter + 1) jumpCounter

execute (PUSHK x) stack _ programCounter jumpCounter = Response.StackMachineResponse (x : stack) (programCounter + 1) jumpCounter

execute ADD (x:y:xs) _ programCounter jumpCounter =
    if isNumber (unpack x) && isNumber (unpack y) then
        Response.StackMachineResponse (pack (show ((P.read (unpack y) :: Int) + (P.read (unpack x) :: Int))) : xs) (programCounter + 1) jumpCounter
    else do
        let y1 = if T.head y == '(' && T.last y == ')'
                then T.init $ T.tail y
                else y
        let x1 = if T.head x == '(' && T.last x == ')'
                then T.init $ T.tail x
                else x
        Response.StackMachineResponse (concat ["(", y1, "+", x1, ")"] : xs) (programCounter + 1) jumpCounter
execute ADD _ _ _ _ = error "Nicht genug Elemente im Stack"

execute SUBTRACT (x:y:xs) _ programCounter jumpCounter
    | isNumber (unpack x) && isNumber (unpack y) = Response.StackMachineResponse (pack (show ((P.read (unpack y) :: Int) - (P.read (unpack x) :: Int))) : xs) (programCounter + 1) jumpCounter
    | otherwise = Response.StackMachineResponse (concat ["(", y, "-", x, ")"] : xs) (programCounter + 1) jumpCounter
execute SUBTRACT _ _ _ _ = error "Nicht genug Elemente im Stack"

execute MULTIPLY (x:y:xs) _ programCounter jumpCounter =
    if isNumber (unpack x) && isNumber (unpack y) then
        Response.StackMachineResponse (pack (show ((P.read (unpack y) :: Int) * (P.read (unpack x) :: Int))) : xs) (programCounter + 1) jumpCounter
    else
        Response.StackMachineResponse (concat [y, "*", x] : xs) (programCounter + 1) jumpCounter
execute MULTIPLY _ _ _ _ = error "Nicht genug Elemente im Stack"

execute DIVIDE (x:y:xs) _ programCounter jumpCounter =
    if isNumber (unpack x) && isNumber (unpack y) then
        Response.StackMachineResponse (pack (show ((P.read (unpack y) :: Int) `div` (P.read (unpack x) :: Int))) : xs) (programCounter + 1) jumpCounter
    else
        Response.StackMachineResponse (concat [y, "/", x] : xs) (programCounter + 1) jumpCounter
execute DIVIDE _ _ _ _ = error "Nicht genug Elemente im Stack"

execute PRINT stack _ programCounter jumpCounter = Response.StackMachineResponse (P.tail stack) (programCounter + 1) jumpCounter
execute (SLIDE (m:n:_)) stack _ programCounter jumpCounter =
    if (m + n) < (length stack + 1) then
        Response.StackMachineResponse (concat [take m stack, drop (m + n) stack]) (programCounter + 1) jumpCounter
    else
        error "Nicht genug Elemente im Stack"
execute (SLIDE _) _ _ _ _ = error "Nicht genug Elemente im Stack"
execute (MARK _) stack _ programCounter jumpCounter = Response.StackMachineResponse stack (programCounter + 1) jumpCounter
execute (JUMP x) stack program _ jumpCounter = Response.StackMachineResponse stack (findMark program (MARK x)) (jumpCounter + 1)
execute (BRANCHZ _) [] _ _ _ = error "Stack enthaelt kein Element"
execute (BRANCHZ x) (y:xs) program programCounter jumpCounter =
    if y == "0" then
        Response.StackMachineResponse xs (findMark program (MARK x)) (jumpCounter + 1)
    else
        Response.StackMachineResponse xs (programCounter + 1) (jumpCounter + 1)

isNumber :: String -> Bool
isNumber ('-':xs) = all isDigit xs
isNumber x = all isDigit x

findMark:: Program -> Command -> Int
findMark program mark =
    if mark `L.elem` program then
        M.fromJust (L.elemIndex mark program)
    else
        error "Sprungmarke nicht gefunden"