module Handler.ExportXls where

-- I read this study that underscores over camelcase is better for legibilty
-- I'm trying it out.

import Import
import Text.XML.SpreadsheetML.Builder
import Text.XML.SpreadsheetML.Writer
import Text.XML.SpreadsheetML.Types

getExportXlsR :: Handler Html
getExportXlsR = do
  (Entity uid _) <- requireAuth
  myPayslips <- runDB $ selectList [PayslipOwner ==. uid] [Asc PayslipId]
  myProcessed <- runDB $ selectList [ProcessedOwner ==. uid] [Asc ProcessedPayslip]
  let slips = fetcthIndividual myPayslips
      proce = fetcthIndividual myProcessed
      tuple_list = makeTupleList slips proce -- [(Payslip, Processed)]
  defaultLayout $ do
    setTitle "Payroll"


fetcthIndividual :: [Entity value] -> [value]
fetcthIndividual [] = []
fetcthIndividual ((Entity _ p): xs) = p : fetcthIndividual xs


makeTupleList :: [Payslip] -> [Processed] -> [(Payslip, Processed)]
makeTupleList [] _ = []
makeTupleList _ [] = []
makeTupleList (x:xs) (y:ys) = (x,y) : makeTupleList xs ys

stringList :: [String] -> [Cell]
stringList [] = []
stringList (x:xs) = string x : stringList xs
