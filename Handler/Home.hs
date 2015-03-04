module Handler.Home where

import Import

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
  let slips = etPayslips myPayslips
      proce = etPayslips myProcessed
      br = b slips proce
      ((Entity payId _):_) = myPayslips
      tSlips = mconcatSlip uid slips
      tProc = mconcatProc uid payId proce
  defaultLayout $ do
    setTitle "Payroll"
    $(widgetFile "homepage")

b :: [Payslip] -> [Processed] -> [(Payslip, Processed)]
b [] _ = []
b _ [] = []
b (x:xs) (y:ys) = (x,y) : b xs ys

etPayslips :: [Entity _t] -> [_t]
etPayslips [] = []
etPayslips ((Entity _ p): xs) = p : etPayslips xs

memptySlip :: UserId -> Payslip 
memptySlip u = Payslip 0 0 0 0 0 u

memptyProc :: UserId -> PayslipId -> Processed
memptyProc u p = Processed 0 0 0 0 0 0 0 0 p u

mappendSlip :: UserId -> Payslip -> Payslip -> Payslip
mappendSlip u (Payslip e w a d i _) (Payslip x y z j h _) = Payslip (e+x) (w+y) (a+z) (d+j) (i+h) u

mappendProc :: UserId -> PayslipId -> Processed -> Processed -> Processed
mappendProc u p (Processed a j c d e f g h _ _) (Processed m n o z q r s t _ _) =
  Processed (a+m) (j+n) (c+o) (d+z) (e+q) (f+r) (g+s) (h+t) p u

mconcatSlip :: UserId -> [Payslip] -> Payslip
mconcatSlip u xs = foldr (\x acc -> mappendSlip u acc x) (memptySlip u) xs

mconcatProc :: UserId -> PayslipId -> [Processed] -> Processed
mconcatProc u p xs = foldr (\x acc -> mappendProc u p acc x) (memptyProc u p) xs
