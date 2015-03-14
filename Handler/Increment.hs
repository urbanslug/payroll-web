module Handler.Increment where

import Import
import Yesod.Form.Bootstrap3
import Data.Time.Calendar (addDays)
import Lib.AssistDB
import Lib.ProcessPayslip 

-- | Form for entering salary increments.
inrementForm :: UserId -> AForm Handler Increment
inrementForm uid = Increment
  <$> areq textField (bfs ("Employee Range" :: Text)) Nothing
  <*> areq doubleField (bfs ("Basic Increment" :: Text)) Nothing
  <*> areq doubleField (bfs ("Allowance Increment" :: Text)) Nothing
  <*> areq doubleField (bfs ("Deduction Increment" :: Text)) Nothing
  <*> pure uid
  <*  bootstrapSubmit (BootstrapSubmit {bsClasses="btn btn-default", bsValue="submit", bsAttrs=[("attr-name", "attr-value")]} :: BootstrapSubmit Text)

getIncrementR :: Handler Html
getIncrementR = do
  (uid, _) <- startValues
  (widget, enctype) <- generateFormPost $ renderBootstrap3 (BootstrapHorizontalForm (ColSm 0) (ColSm 4) (ColSm 0) (ColSm 6)) $ inrementForm uid
  defaultLayout $ do
    setTitle "Increments"
    $(widgetFile "increment")

postIncrementR :: Handler Html
postIncrementR = do
  (uid, day) <- startValues
  ((result, widget), enctype) <- runFormPost $ renderBootstrap3 (BootstrapHorizontalForm (ColSm 0) (ColSm 4) (ColSm 0) (ColSm 6)) $ inrementForm uid
  case result of
   FormSuccess increment -> do
     lastMonth <- return $ addDays (-30) day
     payslipsFromDB <- runDB $ selectList [PayslipOwner ==. uid, PayslipCreatedOn >=. lastMonth ] []
     let payslipList = getValue payslipsFromDB
         listOfSlips = performIncrement increment payslipList
     _ <- mapM save listOfSlips
     redirect $ HomeR
   _ -> defaultLayout $(widgetFile "increment")

