module Handler.PayslipShow where

import Import

getPayslipShowR :: PayslipId ->  Handler Html
getPayslipShowR payslipId = do
  payslip <- runDB $ get404 payslipId
  defaultLayout $ do
    $(widgetFile "payslip/show")
    
-- deletePayslipShowR :: moand m => PayslipId -> ReaderT (PersistEntityBackend Payslip) m ()
deletePayslipShowR :: PayslipId -> Handler Html
deletePayslipShowR payslipId = do
  runDB $ delete payslipId
  defaultLayout $ do
    $(widgetFile "payslip/deleted")
