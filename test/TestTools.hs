module TestTools (
    assertFailure,
    urlPath,
    needsLogin,
    doLogin,
    StdMethod(..)
) where

import TestImport
import Yesod.Core         (RedirectUrl)
import Network.URI        (URI(uriPath), parseURI)
import Network.HTTP.Types (StdMethod(..), renderStdMethod, Status(..))
import Network.Wai.Test   (SResponse(..))

-- Adjust as necessary to the url prefix in the Testing configuration
testRoot :: ByteString
testRoot = "http://localhost:3000"

-- Adjust as necessary for the expected path part of the URL after login
afterLogin :: ByteString
afterLogin = ""

-- Force failure by swearing that black is white, and pigs can fly...
assertFailure :: String -> YesodExample App ()
assertFailure msg = assertEqual msg True False

-- Convert an absolute URL (eg extracted from responses) to just the path
-- for use in test requests.
urlPath :: Text -> Text
urlPath = pack . maybe "" uriPath . parseURI . unpack

-- Internal use only - actual urls are ascii, so exact encoding is irrelevant
urlPathB :: ByteString -> Text
urlPathB = urlPath . decodeUtf8

-- Stages in login process, used below
firstRedirect :: RedirectUrl App url =>
                 StdMethod -> url -> YesodExample App (Maybe ByteString)
firstRedirect method url = do
    request $ do
        setMethod $ renderStdMethod method
        setUrl url
    extractLocation  -- We should get redirected to the login page

assertLoginPage :: ByteString -> YesodExample App ()
assertLoginPage loc = do
    assertEqual "correct login redirection location"
                (testRoot ++ "/auth/login") loc
    get $ urlPathB loc
    statusIs 200
    bodyContains "Login"

submitLogin :: Text -> Text -> YesodExample App (Maybe ByteString)
submitLogin user pass = do
    -- Ideally we would extract this url from the login form on the current page
    request $ do
        setMethod "POST"
        setUrl $ urlPathB $ testRoot ++ "/auth/page/hashdb/login"
        addPostParam "username" user
        addPostParam "password" pass
    extractLocation  -- Successful login should redirect to the home page

extractLocation :: YesodExample App (Maybe ByteString)
extractLocation = do
    withResponse ( \ SResponse { simpleStatus = s, simpleHeaders = h } -> do
                        let code = statusCode s
                        assertEqual ("Expected a 302 or 303 redirection status "
                                     ++ "but received " ++ show code)
                                    (code `oelem` [302,303])
                                    True
                        return $ lookup "Location" h
                 )

-- Check that accessing the url with the given method requires login, and
-- that it redirects us to what looks like the login page.  Note that this is
-- *not* an ajax request, whatever the method, so the redirection *should*
-- result in the HTML login page.
--
needsLogin :: RedirectUrl App url => StdMethod -> url -> YesodExample App ()
needsLogin method url = do
    mbloc <- firstRedirect method url
    maybe (assertFailure "Should have location header") assertLoginPage mbloc

-- Do a login (using hashdb auth).  This just attempts to go to the home
-- url, and follows through the login process.  It should probably be the
-- first thing in each "it" spec.
--
doLogin :: Text -> Text -> YesodExample App ()
doLogin user pass = do
    mbloc <- firstRedirect GET $ urlPathB testRoot
    maybe (assertFailure "Should have location header") assertLoginPage mbloc
    mbloc2 <- submitLogin user pass
    maybe (assertFailure "Should have second location header")
          (assertEqual "Check after-login redirection" $ testRoot ++ afterLogin)
          mbloc2
