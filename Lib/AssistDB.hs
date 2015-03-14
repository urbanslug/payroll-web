{-|
Module      : Lib.AssistDB
Description : Functions that help with the database.
Copyright   : (c) Njagi Mwaniki, 2015
License     : GPL-3
Maintainer  : mwanikibusiness@gmail.com
Stability   : Experimental
Portability : See README


Contains functions that are repeated over different handlers that involve interfacing Yesod and the data from the database.

-}


module Lib.AssistDB where

import Import
import Lib.ProcessPayslip
import Data.Time.Calendar


-- | Get a list of Value from a list of Entity Value.
--   In this case we get Payslip and Processed.
getValue :: [Entity value] -> [value]
getValue [] = []
getValue ((Entity _ p): xs) = p : getValue xs

-- | Creates a list of tuples made up of:
--   a payslip and the associated processed.
makeTupleList :: [Payslip] -> [Processed] -> [(Payslip, Processed)]
makeTupleList [] _ = []
makeTupleList _ [] = []
makeTupleList (x:xs) (y:ys) = (x,y) : makeTupleList xs ys

save :: Payslip -> Handler Payslip
save slip@(Payslip e pbSal pal pded i cr o) = do
  slipId <- runDB $ insert (Payslip e pbSal pal pded i (addDays 30 cr) o)
  processed <- processPayslipM slip slipId
  processedId <- runDB $ insert processed
  return slip
