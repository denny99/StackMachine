module Handler.StackMachineAdmin where

import Import

getStackMachineAdminR :: Handler Html
getStackMachineAdminR = do
    setSession "key" siteSecret
    defaultLayout $ do
        setTitle "Aufgaben"
        $(widgetFile "stackMachineAdmin/stackMachineAdmin")
