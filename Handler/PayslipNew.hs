module Handler.PayslipNew where

import Import
import Yesod.Form.Bootstrap3

payslipForm :: Key User -> AForm Handler Payslip
payslipForm uid  =  Payslip
  <$> areq intField (bfs ("Employee Number" :: Text)) Nothing
  <*> areq intField (bfs ("Basic Salary" :: Text)) Nothing
  <*> areq intField (bfs ("Allowances" :: Text))  Nothing
  <*> areq intField (bfs ("Deductions" :: Text))  Nothing
  <*> areq intField (bfs ("NSSF" :: Text))  Nothing
  <*> areq intField (bfs ("NHIF" :: Text))  Nothing
  <*> pure uid
  <*  bootstrapSubmit (BootstrapSubmit {bsClasses="btn btn-default", bsValue="submit", bsAttrs=[("attr-name", "attr-value")]} :: BootstrapSubmit Text)
 

getPayslipNewR :: Handler Html
getPayslipNewR = do
  (Entity uid _ ) <- requireAuth
  (widget, enctype) <- generateFormPost $ renderBootstrap3 (BootstrapHorizontalForm (ColSm 0) (ColSm 4) (ColSm 0) (ColSm 6)) $ payslipForm uid
  defaultLayout $ do
  setTitle "Add payslip"
  $(widgetFile "payslip/new")


postPayslipNewR :: Handler Html
postPayslipNewR = do
  (Entity uid _ ) <- requireAuth
  ((result, widget), enctype) <- runFormPost $ renderBootstrap3 (BootstrapHorizontalForm (ColSm 0) (ColSm 4) (ColSm 0) (ColSm 6)) $ payslipForm uid
  case result of
   FormSuccess payslip -> do
       payslipId <- runDB $ insert payslip
       redirect $ PayslipShowR payslipId
   _ -> defaultLayout $(widgetFile "payslip/new")

