module Handler.ExportXls where

-- I read this study that underscores over camelcase is better for legibilty
-- I'm trying it out.

import Import
import qualified Data.Text as Text
import Text.XML.SpreadsheetML.Builder
import Text.XML.SpreadsheetML.Writer
import Text.XML.SpreadsheetML.Types
import Control.Monad.IO.Class
import Control.Monad
import Data.Time.Clock

getExportXlsR :: Handler Html
getExportXlsR = do
  (Entity uid _) <- requireAuth
  myPayslips <- runDB $ selectList [PayslipOwner ==. uid] [Asc PayslipId]
  myProcessed <- runDB $ selectList [ProcessedOwner ==. uid] [Asc ProcessedPayslip]
  day <- liftIO $ liftM utctDay getCurrentTime 
  let slips = fetcthIndividual myPayslips
      proce = fetcthIndividual myProcessed
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

-- (Payslip, Processed) -> Row

fetcthIndividual :: [Entity value] -> [value]
fetcthIndividual [] = []
fetcthIndividual ((Entity _ p): xs) = p : fetcthIndividual xs


makeTupleList :: [Payslip] -> [Processed] -> [(Payslip, Processed)]
makeTupleList [] _ = []
makeTupleList _ [] = []
makeTupleList (x:xs) (y:ys) = (x,y) : makeTupleList xs ys

heading :: Row
heading = mkRow $ map string list

mkRowList :: [(Payslip, Processed)] -> [Row]
mkRowList [] = []
mkRowList ((pay,pro): xs) = (mkRow (map string (map show $ 
  [ payslipEmployeeNumber pay
  , payslipBasicSalary pay
  , payslipAllowances pay
  , processedTaxableBenefits pro
  , payslipDeductions pay
  , processedNssf pro
  , processedNhif pro
  , processedTaxableIncome pro
  , processedTaxThereOn pro
  , processedPersonalRelief pro
  , payslipInsuranceRelief pay
  , processedPaye pro
  , processedNetSalary pro
  ]))) : mkRowList xs

stringList :: [String] -> [Cell]
stringList [] = []
stringList (x:xs) = string x : stringList xs

list :: [String]
list =  ["Employee Number", "Basic Salary", "Allowances", "Taxable Benefits", "Deductions", "NSSF", "NHIF", "Taxable Income", "Tax Thereon", "Personal", "Insurance", "PAYE", "Net Salary"]

genWorkbook :: [Row] -> Workbook
genWorkbook = mkWorkbook . singleWorksheet . mkWorksheet (Name "example") . mkTable


singleWorksheet :: Worksheet -> [Worksheet]
singleWorksheet x = x: []
{-
n :: [Cell] -> Workbook
n cell_list = mkWorkbook $ (mkWorksheet (Name "example") $ mkTable $ (mkRow cell_list):[]):[]
-}
toFile :: Workbook -> HandlerT App IO ()
toFile work_book = (writeFile "static/example.xls" $ showSpreadsheet work_book) >> return ()

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
