module Handler.Home where

import Import
import Control.Monad.IO.Class
import Control.Monad (liftM)
import Data.Time.Clock
import Data.Time.Calendar
import Payroll
import Lib.GenTotals
import Lib.AssistDB
import Lib.ProcessPayslip

getHomeR :: Handler Html
getHomeR = do
  (Entity uid _) <- requireAuth
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
  (Entity uid _) <- requireAuth
  lastMonth <- liftIO $ liftM (addDays (-30)) $ liftM utctDay getCurrentTime
  payslipsFromDB <- runDB $ selectList [PayslipOwner ==. uid, PayslipCreatedOn >=. lastMonth ] []
  let listOfSlips = getValue payslipsFromDB
  _ <- mapM save listOfSlips
  redirect HomeR  
