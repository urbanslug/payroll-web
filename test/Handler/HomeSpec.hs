module Handler.HomeSpec (spec) where

import TestImport
import Yesod.Auth(Route(LoginR))
import TestTools

spec :: Spec
spec = withApp $ do
    it "requires login" $ do
      needsLogin GET ("/" :: Text)

    it "creates users" $ do
      get SignUpR
      statusIs 200

      request $ do
        setMethod "POST"
        setUrl SignUpR
        addToken
        
        byLabel "username" "user"
        byLabel "password" "p"
      statusIs 303 -- redirects after successful signup

    it "logs in a user" $ do
      doLogin "user" "p"
