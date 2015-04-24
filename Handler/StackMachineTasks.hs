module Handler.StackMachineTasks where

import Import
import Model.StackMachineTask()

getStackMachineTasksR :: Handler Value
getStackMachineTasksR = do
    tasks <- runDB $ selectList [] [] :: Handler [Entity StackMachineTask]
    return $ object ["tasks" .= tasks]

postStackMachineTasksR :: Handler Html
postStackMachineTasksR = do
    task <- requireJsonBody :: Handler StackMachineTask
    _    <- runDB $ insert task

    sendResponseStatus status201 ("CREATED" :: Text)
