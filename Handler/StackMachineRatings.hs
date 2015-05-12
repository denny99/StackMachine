module Handler.StackMachineRatings where

import Import
import Model.StackMachineRating()

getStackMachineRatingsR :: Handler Html
getStackMachineRatingsR = do
    ratings <- runDB $ selectList [] [] :: Handler [Entity StackMachineRating]
    defaultLayout $ do
        setTitle "Auswertung"
        $(widgetFile "stackMachineRating/stackMachineRating")

postStackMachineRatingsR :: Handler Value
postStackMachineRatingsR = do
    auth <- isAPIAuthenticated
    if not auth
        then
            sendResponseStatus status401 ("NOT AUTHORIZED" :: Text)
        else do
            rating <- requireJsonBody :: Handler StackMachineRating
            _    <- runDB $ insert rating

            sendResponseStatus status201 ("CREATED" :: Text)
