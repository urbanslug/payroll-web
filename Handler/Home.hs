module Handler.Home where

import Import
import Control.Monad.IO.Class
import Control.Monad (liftM)
import Data.Time.Clock
import Data.Time.Calendar
import Payroll

-- import Foundation

-- import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
--                              withSmallInput)

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.

getHomeR :: Handler Html
getHomeR = do
  (Entity uid _) <- requireAuth
  myPayslips <- runDB $ selectList [PayslipOwner ==. uid] [Asc PayslipId]
  myProcessed <- runDB $ selectList [ProcessedOwner ==. uid] [Asc ProcessedPayslip]
  day <- liftIO $ liftM utctDay getCurrentTime 
  let slips = etPayslips myPayslips
      proce = etPayslips myProcessed
      br = b slips proce
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
  let listOfSlips = etPayslips payslipsFromDB
  _ <- mapM save listOfSlips
  redirect HomeR  


-- ----------------------------
save :: Payslip -> Handler Payslip
save x@(Payslip e pbSal pal pded i cr o) = do
  slipId <- runDB $ insert (Payslip e pbSal pal pded i (addDays 30 cr) o)
  processed <- processPayslipM x slipId
  processedId <- runDB $ insert processed
  return x

b :: [Payslip] -> [Processed] -> [(Payslip, Processed)]
b [] _ = []
b _ [] = []
b (x:xs) (y:ys) = (x,y) : b xs ys

etPayslips :: [Entity _t] -> [_t]
etPayslips [] = []
etPayslips ((Entity _ p): xs) = p : etPayslips xs

memptySlip :: Day -> UserId -> Payslip 
memptySlip d u = Payslip 0 0 0 0 0 d u

memptyProc :: UserId -> PayslipId -> Processed
memptyProc u p = Processed 0 0 0 0 0 0 0 0 p u

mappendSlip :: Day -> UserId -> Payslip -> Payslip -> Payslip
mappendSlip c u (Payslip e w a d i _ _) (Payslip x y z j h _ _) = Payslip (e+x) (w+y) (a+z) (d+j) (i+h) c u

mappendProc :: UserId -> PayslipId -> Processed -> Processed -> Processed
mappendProc u p (Processed a j c d e f g h _ _) (Processed m n o z q r s t _ _) =
  Processed (a+m) (j+n) (c+o) (d+z) (e+q) (f+r) (g+s) (h+t) p u

mconcatSlip :: Day -> UserId -> [Payslip] -> Payslip
mconcatSlip c u xs = foldr (\x acc -> mappendSlip c u acc x) (memptySlip c u) xs

mconcatProc :: UserId -> PayslipId -> [Processed] -> Processed
mconcatProc u p xs = foldr (\x acc -> mappendProc u p acc x) (memptyProc u p) xs

processPayslipM :: (Monad m) => Payslip -> PayslipId -> m Processed
processPayslipM p pId = return $ processPayslip p pId

processPayslip :: Payslip -> PayslipId -> Processed
processPayslip payslip@(Payslip _ basicSalary allowances deductions insuranceRelief _ _) payslipId =
  let taxableB = taxableBenefits basicSalary [allowances]
      taxableI = taxableIncome taxableB [deductions]
      tThereon = taxThereOn taxableI
      tPaye = paye tThereon insuranceRelief
      netSal = netSalary taxableI tPaye
  in  Processed { processedTaxableBenefits = taxableB, 
                  processedTaxableIncome = taxableI,
                  processedTaxThereOn = tThereon,
                  processedNssf = nssf,
                  processedNhif = nhif,
                  processedPersonalRelief = personalRelief,
                  processedPaye = tPaye,
                  processedNetSalary = netSal,
                  processedPayslip = payslipId,
                  processedOwner = payslipOwner payslip
                }
