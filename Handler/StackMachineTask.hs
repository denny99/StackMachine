module Handler.StackMachineTask where

import Import
import Model.StackMachineTask()

getStackMachineTaskR :: StackMachineTaskId -> Handler Value
getStackMachineTaskR stackMachineTaskId = do
    task <- runDB $ get404 stackMachineTaskId
    return $ object ["task" .= Entity stackMachineTaskId task]

putStackMachineTaskR :: StackMachineTaskId -> Handler Value
putStackMachineTaskR stackMachineTaskId = do
    task <- requireJsonBody :: Handler StackMachineTask
    runDB $ replace stackMachineTaskId task
    sendResponseStatus status200 ("UPDATED" :: Text)

deleteStackMachineTaskR :: StackMachineTaskId -> Handler Value
deleteStackMachineTaskR stackMachineTaskId = do
    runDB $ delete stackMachineTaskId
    sendResponseStatus status200 ("DELETED" :: Text)
