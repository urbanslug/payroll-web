module Handler.PayslipShow where

import Import

getPayslipShowR :: PayslipId ->  Handler Html
getPayslipShowR payslipId = do
  (uid, _) <- startValues
  payslip <- runDB $ get404 payslipId
  (Entity _ processed) <- runDB $ getBy404 $ UniquePayslip payslipId
  defaultLayout $ do
       $(widgetFile "payslip/show")

deletePayslipShowR :: PayslipId -> Handler ()
deletePayslipShowR payslipId = do
  (uid, _) <- startValues
  maybePayslip <- runDB $ get payslipId
  case maybePayslip of
   Just payslip -> do
     case ((payslipOwner payslip) == uid) of
      True -> runDB $ do
        -- | delete processed first or else foreign key constraints won't let you delete a payslip.
              deleteBy $ UniquePayslip payslipId 
              delete payslipId
      False -> return ()
   Nothing -> return ()
