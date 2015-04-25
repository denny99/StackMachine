module Handler.StackMachineAdmin where

import Import

getStackMachineAdminR :: Handler Html
getStackMachineAdminR = do
    print cred
    setSession "key" siteSecret
    defaultLayout $(widgetFile "stackMachineAdmin")
