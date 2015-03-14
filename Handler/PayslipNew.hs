module Handler.PayslipNew where

import Import
import Yesod.Form.Bootstrap3
import Control.Monad.IO.Class
import Control.Monad
import Data.Time.Clock
import Payroll
import Lib.ProcessPayslip

payslipForm :: Day -> Key User -> AForm Handler Payslip
payslipForm day uid  =  Payslip
  <$> areq intField (bfs ("Employee Number" :: Text)) Nothing
  <*> areq intField (bfs ("Basic Salary" :: Text)) Nothing
  <*> areq intField (bfs ("Allowances" :: Text))  Nothing
  <*> areq intField (bfs ("Deductions" :: Text))  Nothing
  <*> areq intField (bfs ("Insurance Relief" :: Text))  Nothing
  <*> pure day
  <*> pure uid
  <*  bootstrapSubmit (BootstrapSubmit {bsClasses="btn btn-default", bsValue="submit", bsAttrs=[("attr-name", "attr-value")]} :: BootstrapSubmit Text)
 

getPayslipNewR :: Handler Html
getPayslipNewR = do
  (Entity uid _ ) <- requireAuth
  day <- liftIO $ liftM utctDay getCurrentTime 
  (widget, enctype) <- generateFormPost $ renderBootstrap3 (BootstrapHorizontalForm (ColSm 0) (ColSm 4) (ColSm 0) (ColSm 6)) $ payslipForm day uid
  defaultLayout $ do
  setTitle "Add payslip"
  $(widgetFile "payslip/new")


postPayslipNewR :: Handler Html
postPayslipNewR = do
  (Entity uid _ ) <- requireAuth
  day <- liftIO $ liftM utctDay getCurrentTime 
  ((result, widget), enctype) <- runFormPost $ renderBootstrap3 (BootstrapHorizontalForm (ColSm 0) (ColSm 4) (ColSm 0) (ColSm 6)) $ payslipForm day uid
  case result of
   FormSuccess payslip -> do
       payslipId <- runDB $ insert payslip
       processed <- processPayslipM payslip payslipId
       _ <- runDB $ insert processed
       redirect $ PayslipShowR payslipId
   _ -> defaultLayout $(widgetFile "payslip/new")

