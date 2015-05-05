module Handler.StackMachineTasks where

import Import
import Data.Maybe
import Model.StackMachineTask()

getStackMachineTasksR :: Handler Value
getStackMachineTasksR = do
    auth <- isAPIAuthenticated
    publicMaybe <- lookupGetParam "public"
    if not auth then
            sendResponseStatus status401 ("NOT AUTHORIZED" :: Text)
        else
            if isJust publicMaybe
                then
                    if fromJust publicMaybe == "true"
                        then do
                            tasks <- runDB $ selectList [StackMachineTaskActive ==. True] []
                            return $ object ["tasks" .= tasks]
                        else do
                            tasks <- runDB $ selectList [] [] :: Handler [Entity StackMachineTask]
                            return $ object ["tasks" .= tasks]
                else
                    sendResponseStatus status400 ("INVALID QUERY PARAMETER" :: Text)

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
