module Handler.StackMachineAdmin where

import Import

getStackMachineAdminR :: Handler Html
getStackMachineAdminR = defaultLayout $(widgetFile "stackMachineAdmin")
