module Handler.Increment where

import Import
import Yesod.Form.Bootstrap3
import Payroll
import Data.Time.Calendar
import Data.Time.Clock
import Control.Monad (liftM)
import Control.Monad.IO.Class

-- | Form for inputting increments.
inrementForm :: UserId -> AForm Handler Increment
inrementForm uid = Increment
  <$> areq textField (bfs ("Employee Range" :: Text)) Nothing
  <*> areq doubleField (bfs ("Basic Increment" :: Text)) Nothing
  <*> areq doubleField (bfs ("Allowance Increment" :: Text)) Nothing
  <*> areq doubleField (bfs ("Deduction Increment" :: Text)) Nothing
  <*> pure uid
  <*  bootstrapSubmit (BootstrapSubmit {bsClasses="btn btn-default", bsValue="submit", bsAttrs=[("attr-name", "attr-value")]} :: BootstrapSubmit Text)

getIncrementR :: Handler Html
getIncrementR = do
  (Entity uid _) <- requireAuth
  (widget, enctype) <- generateFormPost $ renderBootstrap3 (BootstrapHorizontalForm (ColSm 0) (ColSm 4) (ColSm 0) (ColSm 6)) $ inrementForm uid
  defaultLayout $ do
    setTitle "Increments"
    $(widgetFile "increment")

postIncrementR :: Handler Html
postIncrementR = do
  (Entity uid _) <- requireAuth
  ((result, widget), enctype) <- runFormPost $ renderBootstrap3 (BootstrapHorizontalForm (ColSm 0) (ColSm 4) (ColSm 0) (ColSm 6)) $ inrementForm uid
  case result of
   FormSuccess increment -> do
     lastMonth <- liftIO $ liftM (addDays (-30)) $ liftM utctDay getCurrentTime
     payslipsFromDB <- runDB $ selectList [PayslipOwner ==. uid, PayslipCreatedOn >=. lastMonth ] []
     let payslipList = extractPayslips payslipsFromDB
         listOfSlips = runIncrement increment payslipList
     _ <- mapM save listOfSlips
     redirect $ HomeR
   _ -> defaultLayout $(widgetFile "increment")


save :: Payslip -> Handler Payslip
save x = do
  slipId <- runDB $ insert x
  processed <- processPayslipM x slipId
  processedId <- runDB $ insert processed
  return x
  

extractPayslips :: [Entity Payslip] -> [Payslip]
extractPayslips [] = []
extractPayslips ((Entity _ payslip): xs) = payslip : extractPayslips xs

runIncrement :: Increment -> [Payslip] -> [Payslip]
runIncrement _ [] = []
runIncrement inc@(Increment _ ibSal ial ided _) ((Payslip e pbSal pal pded i cr o):xs) =
  (Payslip e (ibSal % pbSal) (ial % pal) (ided % pded) i (addDays 30 cr) o): runIncrement inc xs


-- | See it as x % of y
(%) :: Double -> Int -> Int
(%) x y = round $ ((100.0 + x)/100) * fromIntegral y
  
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
