{-|
Module      : Lib.GenTotals
Description : Generate totals for a single user payroll.
Copyright   : (c) Njagi Mwaniki, 2015
License     : GPL-3
Maintainer  : mwanikibusiness@gmail.com
Stability   : Experimental
Portability : See README

Use a monoid like structure to generate totals from Payslip and Processed values.
These totals are used to create a totals summary of the payroll.
I plan to make these real monoids.
-}

module Lib.GenTotals where

import Import
import Data.Time.Calendar


-- | mempty instance for a Payslip value
memptySlip :: Day -> UserId -> Payslip 
memptySlip d u = Payslip 0 0 0 0 0 d u

-- | mappend instance for Payslip values
mappendSlip :: Day -> UserId -> Payslip -> Payslip -> Payslip
mappendSlip c u (Payslip e w a d i _ _) (Payslip x y z j h _ _) = Payslip (e+x) (w+y) (a+z) (d+j) (i+h) c u

-- | mconcat instance for Payslip values 
mconcatSlip :: Day -> UserId -> [Payslip] -> Payslip
mconcatSlip c u xs = foldr (\x acc -> mappendSlip c u acc x) (memptySlip c u) xs

-- For Processed


-- | mempty instance for a Processed value
memptyProc :: UserId -> PayslipId -> Processed
memptyProc u p = Processed 0 0 0 0 0 0 0 0 p u

-- | mappend instance for Payslip values
mappendProc :: UserId -> PayslipId -> Processed -> Processed -> Processed
mappendProc u p (Processed a j c d e f g h _ _) (Processed m n o z q r s t _ _) =
  Processed (a+m) (j+n) (c+o) (d+z) (e+q) (f+r) (g+s) (h+t) p u

-- | mconcat instance for Payslip values 
mconcatProc :: UserId -> PayslipId -> [Processed] -> Processed
mconcatProc u p xs = foldr (\x acc -> mappendProc u p acc x) (memptyProc u p) xs
