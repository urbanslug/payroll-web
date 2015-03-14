module Handler.ExportXls where

import Import
import qualified Data.Text as Text
import Control.Monad.IO.Class
import Control.Monad
import Data.Time.Clock
import Lib.BuildTable
import Lib.GenTotals
import Lib.AssistDB

getExportXlsR :: Handler Html
getExportXlsR = do
  (Entity uid _) <- requireAuth
  myPayslips <- runDB $ selectList [PayslipOwner ==. uid] [Asc PayslipId]
  myProcessed <- runDB $ selectList [ProcessedOwner ==. uid] [Asc ProcessedPayslip]
  day <- liftIO $ liftM utctDay getCurrentTime 
  let slips = getValue myPayslips
      proce = getValue myProcessed
      tupleList = makeTupleList slips proce -- [(Payslip, Processed)]
      ((Entity payId _):_) = myPayslips
      totalSlips = mconcatSlip day uid slips
      totalProc = mconcatProc uid payId proce
  _ <- toFile $ genWorkbook $ heading : mkRowList tupleList ++ mkRowList [(totalSlips, totalProc)]
  addHeader "Content-Disposition" $ Text.concat
    ["attachment; filename=\"example.xls\""]
  _ <- sendFile "text/xls" "static/example.xls"
  defaultLayout $ do
    setTitle "export"
    $(widgetFile "payslip/exportxls")

