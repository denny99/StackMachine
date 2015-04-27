module Handler.StackMachine where

import Import
import qualified Prelude as P
import qualified Model.StackMachineRequest as Request
import Model.StackMachine

getStackMachineR :: Handler Html
getStackMachineR = do
    setSession "key" siteSecret
    defaultLayout $(widgetFile "stackMachine/stackMachine")

postStackMachineR :: Handler Value
postStackMachineR = do
    stackMachineRequest <- requireJsonBody :: Handler Request.StackMachineRequest
    let program = Request.program stackMachineRequest
    let programCounter = Request.programCounter stackMachineRequest
    let stack = Request.stack stackMachineRequest
    if Request.all stackMachineRequest == False
        then
            if programCounter < length program then
                sendResponseStatus status200 (execute (program P.!! programCounter) stack program programCounter 0)
            else
                error "Programm ist bereits beendet"
        else
            sendResponseStatus status200 (executeAll stack program programCounter 0)