module Handler.Home where

import Import
import Data.Time.Calendar (addDays)
import Lib.GenTotals
import Lib.AssistDB
import Lib.ProcessPayslip ()

getHomeR :: Handler Html
getHomeR = do
  (uid, _) <- startValues
  myPayslips <- runDB $ selectList [PayslipOwner ==. uid] [Asc PayslipId]
  myProcessed <- runDB $ selectList [ProcessedOwner ==. uid] [Asc ProcessedPayslip]
  day <- liftIO $ liftM utctDay getCurrentTime 
  let slips = getValue myPayslips
      proce = getValue myProcessed
      br = makeTupleList slips proce
      ((Entity payId _):_) = myPayslips
      tSlips = mconcatSlip day uid slips
      tProc = mconcatProc uid payId proce
  defaultLayout $ do
    setTitle "Payroll"
    $(widgetFile "homepage")

putHomeR :: Handler Html
putHomeR = do
  (uid, day) <- startValues
  lastMonth <- return $ addDays (-30) day
  payslipsFromDB <- runDB $ selectList [PayslipOwner ==. uid, PayslipCreatedOn >=. lastMonth ] []
  let listOfSlips = getValue payslipsFromDB
  _ <- mapM save listOfSlips
  redirect HomeR  
