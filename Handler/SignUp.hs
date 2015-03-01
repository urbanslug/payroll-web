module Handler.SignUp where

import Import
import Yesod.Form.Bootstrap3
import Yesod.Auth.HashDB (setPassword)

signUpForm :: AForm Handler User
signUpForm = User
  <$> areq textField (bfs ("Username" :: Text)) Nothing
  <*> (Just <$> areq passwordField (bfs ("Password" :: Text)) Nothing)
  <*  bootstrapSubmit (BootstrapSubmit {bsClasses="btn btn-default", bsValue="submit", bsAttrs=[("attr-name", "attr-value")]} :: BootstrapSubmit Text)

getSignUpR :: Handler Html
getSignUpR = do
  (widget, enctype) <- generateFormPost $ renderBootstrap3 (BootstrapHorizontalForm (ColSm 0) (ColSm 4) (ColSm 0) (ColSm 6)) signUpForm
  defaultLayout $ do
  setTitle "Sign up"
  $(widgetFile "signup")

postSignUpR :: Handler Html
postSignUpR = do
  ((result, widget), enctype) <- runFormPost $ renderBootstrap3 (BootstrapHorizontalForm (ColSm 0) (ColSm 4) (ColSm 0) (ColSm 6)) signUpForm
  case result of
   FormSuccess user -> do
     case (userPassword user) of
      Nothing -> defaultLayout $(widgetFile "signup")
      Just password -> do
        -- returns a userId but we don't need that.
        _ <- setPassword password user >>= \uz -> runDB $ insert uz
        redirect HomeR
   _ -> defaultLayout $(widgetFile "signup")
