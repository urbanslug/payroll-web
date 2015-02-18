module Handler.PayslipShow where

import Import

getPayslipShowR :: PayslipId ->  Handler Html
getPayslipShowR payslipId = do
  maybeUser <- maybeAuth
  case maybeUser of
   Just (Entity uid _) -> do
     payslip <- runDB $ get404 payslipId
     defaultLayout $ do
       $(widgetFile "payslip/show")
   Nothing -> do
     defaultLayout $ do
       redirect $ HomeR
    
deletePayslipShowR :: PayslipId -> Handler ()
deletePayslipShowR payslipId = do
  maybeUser <- maybeAuth 
  maybePayslip <- runDB $ get payslipId
  case maybeUser of
   Just (Entity uid _) -> do
     case maybePayslip of
      Just payslip -> do
        case ((payslipOwner payslip) == uid) of
         True -> runDB $ delete payslipId
         False -> return ()
      Nothing -> return ()
   Nothing -> return ()
