module Handler.StackMachineTask where

import Import
import Model.Stack
import Model.StackMachine
import Model.StackMachineTask()
import Model.StackMachineTaskRequest
import Model.StackMachineResponse
import Model.Formula

getStackMachineTaskR :: StackMachineTaskId -> Handler Value
getStackMachineTaskR stackMachineTaskId = do
    auth <- isAPIAuthenticated
    if not auth then
            sendResponseStatus status401 ("NOT AUTHORIZED" :: Text)
        else do
            task <- runDB $ get404 stackMachineTaskId
            return $ object ["task" .= Entity stackMachineTaskId task]

postStackMachineTaskR :: StackMachineTaskId -> Handler Value
postStackMachineTaskR stackMachineTaskId = do
   auth <- isAPIAuthenticated
   if not auth then
           sendResponseStatus status401 ("NOT AUTHORIZED" :: Text)
       else do
           task <- runDB $ get404 stackMachineTaskId
           body <- requireJsonBody :: Handler StackMachineTaskRequest
           let result = map expandFormula (stack (executeAll (arrayToStack (stackMachineTaskInitialStack task)) (program body) 0 0))
           let target = map expandFormula (arrayToStack (stackMachineTaskTargetStack task))
           if result /= target
               then
                   if partialEq result target then sendResponseStatus status400 ("Zu viele Elemente im Stack (Tip: slide)" :: Text)
                   else sendResponseStatus status400 ("Ergebnis fehlerhaft" :: Text)
               else
                   sendResponseStatus status200 ("Korrekt" :: Text)

putStackMachineTaskR :: StackMachineTaskId -> Handler Value
putStackMachineTaskR stackMachineTaskId = do
    auth <- isAPIAuthenticated
    if not auth then
            sendResponseStatus status401 ("NOT AUTHORIZED" :: Text)
        else do
            task <- requireJsonBody :: Handler StackMachineTask
            runDB $ replace stackMachineTaskId task
            sendResponseStatus status200 ("UPDATED" :: Text)

deleteStackMachineTaskR :: StackMachineTaskId -> Handler Value
deleteStackMachineTaskR stackMachineTaskId = do
    auth <- isAPIAuthenticated
    if not auth then
            sendResponseStatus status401 ("NOT AUTHORIZED" :: Text)
        else do
            runDB $ delete stackMachineTaskId
            sendResponseStatus status200 ("DELETED" :: Text)
