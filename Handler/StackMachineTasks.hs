module Handler.StackMachineTasks where

import Import
import Model.StackMachineTask()

getStackMachineTasksR :: Handler Value
getStackMachineTasksR = do
    auth <- isAPIAuthenticated
    if not auth then
            sendResponseStatus status401 ("NOT AUTHORIZED" :: Text)
        else do
            tasks <- runDB $ selectList [] [] :: Handler [Entity StackMachineTask]
            return $ object ["tasks" .= tasks]

postStackMachineTasksR :: Handler Html
postStackMachineTasksR = do
    auth <- isAPIAuthenticated
    if not auth
        then
            sendResponseStatus status401 ("NOT AUTHORIZED" :: Text)
        else do
            task <- requireJsonBody :: Handler StackMachineTask
            _    <- runDB $ insert task

            sendResponseStatus status201 ("CREATED" :: Text)
