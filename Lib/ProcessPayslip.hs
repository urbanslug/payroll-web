{-|
Module      : Lib.ProcessPayslip
Description : Functions that generates a Processed from a payslip.
Copyright   : (c) Njagi Mwaniki, 2015
License     : GPL-3
Maintainer  : mwanikibusiness@gmail.com
Stability   : Experimental
Portability : See README

Functions to interface with the core payroll package.
Used to process a payslip to come up with a Processed value.
-}


module Lib.ProcessPayslip
       ( processPayslipM
       , performIncrement
       )
       where

import Import
import Payroll
import Data.Time.Calendar (addDays)

-- | Exposed to the handlers. Wraps a processed in a monad.
-- Most times the Handler monad.
processPayslipM :: (Monad m) => Payslip -> PayslipId -> m Processed
processPayslipM p pId = return $ processPayslip p pId

-- | Pure function. Interfaces with the payroll core package.
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


-- | Performs the actual increment on a Payslip value
-- Read (%) it as x % of y
-- >>> 5 % 100
-- 5
performIncrement :: Increment -> [Payslip] -> [Payslip]
performIncrement _ [] = []
performIncrement inc@(Increment _ ibSal ial ided _) ((Payslip e pbSal pal pded i cr o):xs) =
  (Payslip e (ibSal % pbSal) (ial % pal) (ided % pded) i (addDays 30 cr) o): performIncrement inc xs
  where (%) :: Double -> Int -> Int
        (%) x y = round $ ((100.0 + x)/100) * fromIntegral y



