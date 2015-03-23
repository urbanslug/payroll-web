{-|
Module      : Lib.BuildTable
Description : Has functions that help "Handler.ExportXls" create an Xls file.
Copyright   : (c) Njagi Mwaniki, 2015
License     : GPL-3
Maintainer  : mwanikibusiness@gmail.com
Stability   : Experimental
Portability : See README

Code that helps in exporting the payroll as an xls file.
-}

{-
The idea is to keep to DRY and adhere to best practices
as much as possible therefore breaking away code that
isn't specific to the handler and may be used by other handlers.
-}

module Lib.BuildTable
       ( mkRowList
       , genWorkbook
       , heading
       , toFile
       , mkRowListT
         )
       where

import Import
import Text.XML.SpreadsheetML.Builder
import Text.XML.SpreadsheetML.Writer
import Text.XML.SpreadsheetML.Types

-- | Creates a heading for the worksheet we will create.
heading :: Row
heading = let header = [ "Employee Number"
                       , "Basic Salary"
                       , "Allowances"
                       , "Taxable Benefits"
                       , "Deductions"
                       , "NSSF"
                       , "NHIF"
                       , "Taxable Income"
                       , "Tax Thereon"
                       , "Personal"
                       , "Insurance"
                       , "PAYE"
                       , "Net Salary"
                       ]
             in mkRow $ map string header

-- Will be removed. It's a quick hack.
mkRowListT :: (Payslip, Processed) -> [Row]
mkRowListT (pay,pro) = [mkRow $ [(string "Total")] ++ (map string $ map show $ 
  [ payslipBasicSalary pay
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
  ])]


-- | A list of rows will be used to make a table.
-- It takes each (Payslip, Processed) tuple and
-- turns it into a row. The list of tuples are
-- written into a list of rows.
mkRowList :: [(Payslip, Processed)] -> [Row]
mkRowList [] = []
mkRowList ((pay,pro): xs) = (mkRow $ map string $ map show $ 
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
  ]) : mkRowList xs


-- | Generate a workbook when given only a list of rows.
genWorkbook :: [Row] -> Workbook
genWorkbook = mkWorkbook . singleWorksheet . mkWorksheet (Name "Payroll") . mkTable

-- | Creates a one element list of worksheets
-- from a single worksheet.
singleWorksheet :: Worksheet -> [Worksheet]
singleWorksheet x = x:[]

-- | Writes the workbook to a file.
-- This file will be downloadable through:
-- 'Handler.ExportXls.getExportXls'
toFile :: Workbook -> HandlerT App IO ()
toFile work_book = (writeFile "static/example.xls" $ showSpreadsheet work_book) >> return ()

