module Handler.Home where

import Import
import Data.Time.Calendar (addDays)
-- import Data.Time.Format
import Lib.GenTotals
import Lib.AssistDB
import Lib.ProcessPayslip ()
import qualified Data.List as List

getHomeR :: Handler Html
getHomeR = do
  (uid, day) <- startValues
  myPayslips <- runDB $ selectList [PayslipOwner ==. uid] [Asc PayslipId]
  myProcessed <- runDB $ selectList [ProcessedOwner ==. uid] [Asc ProcessedPayslip]
  let slips = getValue myPayslips
      proce = getValue myProcessed
      bree = makeTupleList slips proce
      br = groupTupleList $ gg (zz myPayslips) bree
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

gg :: [Key Payslip] -> [(Payslip, Processed)] -> [(PayslipId, Payslip, Processed)]
gg [] _ = []
gg _ [] = []
gg (idd:ys) ((pay,pro):xs) = (idd,pay,pro): gg ys xs

zz :: [Entity Payslip] -> [Key Payslip]
zz [] = []
zz ((Entity payIdd _):xs) = payIdd : zz xs

getMonths :: [[(PayslipId, Payslip, Processed)]] -> [String]
getMonths = map getMonth

-- | Get's month for a list of tuples.
getMonth :: [(PayslipId, Payslip, Processed)] -> String
getMonth [] = []
getMonth ((_, Payslip _ _ _ _ _ d _, _):_) = (formatTime defaultTimeLocale "%Y %b" d)

groupTupleList :: [(PayslipId, Payslip,Processed)] -> [[(PayslipId, Payslip, Processed)]]
groupTupleList = List.groupBy j

j :: (PayslipId, Payslip,Processed) -> (PayslipId, Payslip,Processed) -> Bool
j (_, Payslip _ _ _ _ _ b _, _) (_, Payslip _ _ _ _ _ c _, _) = eqSlip b c


eqSlip :: Day -> Day ->  Bool
eqSlip f s = yearMonth f == yearMonth s
  where yearMonth :: Day -> String
        yearMonth = formatTime defaultTimeLocale "%Y %b"
