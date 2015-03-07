module Handler.HomeSpec (spec) where

import TestImport
import Yesod.Auth(Route(LoginR))

spec :: Spec
spec = withApp $ do
  describe "Auth things" $ do
    it "creates users" $ do
      get SignUpR
      statusIs 200

      request $ do
        byLabel "username" "user"
        byLabel "password" "p"
        setMethod "POST"
        setUrl SignUpR
      statusIs 200
      
    it "logs users in" $ do
      get (AuthR LoginR)
      statusIs 200

      bodyContains "Login"

      request $ do
        byLabel "username" "user"
        byLabel "password" "p"
        setMethod "GET"
        setUrl (AuthR LoginR)
      statusIs 200

    it "user has been logged in" $ do
      get HomeR
      statusIs 303
      bodyContains "Login"
    
      

{-
spec :: Spec
spec = withApp $ do
    it "loads the index and checks it looks right" $ do
        get HomeR
        statusIs 200
        htmlAllContain "h1" "Welcome to Yesod"

        request $ do
            setMethod "POST"
            setUrl HomeR
            addNonce
            fileByLabel "Choose a file" "test/Spec.hs" "text/plain" -- talk about self-reference
            byLabel "What's on the file?" "Some Content"

        statusIs 200
        -- more debugging printBody
        htmlCount ".message" 1
        htmlAllContain ".message" "Some Content"
        htmlAllContain ".message" "text/plain"

    -- This is a simple example of using a database access in a test.  The
    -- test will succeed for a fresh scaffolded site with an empty database,
    -- but will fail on an existing database with a non-empty user table.
    it "leaves the user table empty" $ do
        get HomeR
        statusIs 200
        users <- runDB $ selectList ([] :: [Filter User]) []
        assertEqual "user table empty" 0 $ length users
-}
