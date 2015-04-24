module Handler.StackMachineTask where

import Import
import Prelude (read)
import qualified Data.Text as T
import Model.Stack
import Model.StackMachine
import Model.StackMachineTask()
import Model.StackMachineTaskRequest
import Model.StackMachineResponse

getStackMachineTaskR :: StackMachineTaskId -> Handler Value
getStackMachineTaskR stackMachineTaskId = do
    task <- runDB $ get404 stackMachineTaskId
    return $ object ["task" .= Entity stackMachineTaskId task]

postStackMachineTaskR :: StackMachineTaskId -> Handler Value
postStackMachineTaskR stackMachineTaskId = do
    task <- runDB $ get404 stackMachineTaskId
    body <- requireJsonBody :: Handler StackMachineTaskRequest
    if stack (executeAll (read (T.unpack (stackMachineTaskInitialStack task)) :: Stack) (program body) 0 0) /= (read (T.unpack (stackMachineTaskTargetStack task)) :: Stack)
        then
            sendResponseStatus status400 ("Ergebnis fehlerhaft" :: Text)
        else
            sendResponseStatus status200 ("Korrekt" :: Text)

putStackMachineTaskR :: StackMachineTaskId -> Handler Value
putStackMachineTaskR stackMachineTaskId = do
    task <- requireJsonBody :: Handler StackMachineTask
    runDB $ replace stackMachineTaskId task
    sendResponseStatus status200 ("UPDATED" :: Text)

deleteStackMachineTaskR :: StackMachineTaskId -> Handler Value
deleteStackMachineTaskR stackMachineTaskId = do
    runDB $ delete stackMachineTaskId
    sendResponseStatus status200 ("DELETED" :: Text)
