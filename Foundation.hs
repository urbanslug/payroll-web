module Foundation where

import Import.NoFoundation
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Text.Hamlet          (hamletFile)
import Text.Jasmine         (minifym)
import Yesod.Auth.GoogleEmail2
import Data.Text (splitOn)
import Data.List((!!))
-- import Yesod.Auth.Dummy
import Yesod.Default.Util   (addStaticContentExternal)
import Yesod.Core.Types     (Logger)
import qualified Yesod.Core.Unsafe as Unsafe

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static -- ^ Settings for static file serving.
    , appConnPool    :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger      :: Logger
    }

instance HasHttpManager App where
    getHttpManager = appHttpManager

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the linked documentation for an
-- explanation for this split.
mkYesodData "App" $(parseRoutesFile "config/routes")

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot = ApprootMaster $ appRoot . appSettings

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = fmap Just $ defaultClientSessionBackend
        120    -- timeout in minutes
        "config/client_session_key.aes"

    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            addStylesheet $ StaticR css_bootstrap_css
            $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    -- Routes not requiring authentication.
    isAuthorized (AuthR _) _ = return Authorized
    isAuthorized FaviconR  _ = return Authorized
    isAuthorized RobotsR   _ = return Authorized

    
    -- Everything else requires authorization.
    isAuthorized _ _ = isRegistered

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent ext mime content = do
        master <- getYesod
        let staticDir = appStaticDir $ appSettings master
        addStaticContentExternal
            minifym
            genFileName
            staticDir
            (StaticR . flip StaticRoute [])
            ext
            mime
            content
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs = "autogen-" ++ base64md5 lbs

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog app _source level =
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger = return . appLogger
       
isRegistered :: Handler AuthResult
isRegistered = do
  mauth <- maybeAuth
  case mauth of
       Nothing -> return Authorized
       _ -> return Authorized

{-
addAuthBackDoor :: App -> [AuthPlugin App] -> [AuthPlugin App]
addAuthBackDoor app =
    if appAllowDummyAuth (appSettings app) then (authDummy :) else id
-}
buildUser :: Creds m -> Maybe User
buildUser (Creds csPlugin csIdent csExtra) =
        User <$> lookup "displayName" csExtra
             <*> case lookup "image" csExtra of
                  Just texty -> Just $ (splitOn "\"" texty) !! 3
                  _ -> Nothing
             <*> pure csIdent
             <*> pure csPlugin
             <*> pure csIdent -- | Save email twice because I'm stupid.


replaceUser :: UserId -> Maybe User -> YesodDB App UserId
replaceUser uid (Just u) = replace uid u >> return uid
replaceUser uid _        = return uid

insertUser :: Maybe User -> YesodDB App (Maybe UserId)
insertUser (Just u) = fmap Just $ insert u
insertUser _        = return Nothing

-- To avoid code replication over various handlers.
-- Values needed at the beginning of a handler.
startValues :: Handler (UserId, Day)
startValues = do
  (Entity uid _ ) <- requireAuth
  day <- liftIO $ liftM utctDay getCurrentTime
  return (uid, day)
               
-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner appConnPool

instance YesodAuth App where
    type AuthId App = UserId

    -- Where to send a user after successful login
    loginDest _ = HomeR
    -- Where to send a user after logout
    logoutDest _ = HomeR
    -- Override the above two destinations when a Referer: header is present
    redirectToReferer _ = True

    getAuthId creds = runDB $ do
        muser <- getBy $ UniqueUser (credsPlugin creds) (credsIdent creds)
        _ <- lift $ print $ credsExtra creds -- To see what credsExtra holds.
        let newUser = buildUser creds

        case muser of
            Just (Entity uid _) -> fmap Just $ replaceUser uid newUser
            _                   -> insertUser newUser

    -- You can add other plugins like BrowserID, email or OAuth here
    authPlugins _ = [ authGoogleEmailSaveToken
                      "388063239568-1d6m1gl80fm9dl0nrmk9805af8kul39l.apps.googleusercontent.com"
                      "t01A_GWC-YSXhHYMzUfCz7H6"
                    ]

    authHttpManager = getHttpManager


instance YesodAuthPersist App

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding
