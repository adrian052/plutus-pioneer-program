{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}

module Homework1 where


import Plutus.V1.Ledger.Interval (contains,before)
import Plutus.V2.Ledger.Contexts (txSignedBy)
import Plutus.V2.Ledger.Api
  ( BuiltinData,
    POSIXTime,
    PubKeyHash,
    ScriptContext (scriptContextTxInfo),
    TxInfo (txInfoValidRange),
    Validator,
    from,
    to,
    mkValidatorScript,
  )
import           PlutusTx             (compile, unstableMakeIsData)
import           PlutusTx.Prelude     (Bool (..),($), (&&),(||),not,(>))
import           Utilities            (wrapValidator)

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

data VestingDatum = VestingDatum
    { beneficiary1 :: PubKeyHash
    , beneficiary2 :: PubKeyHash
    , deadline     :: POSIXTime
    }

unstableMakeIsData ''VestingDatum

{-# INLINABLE mkVestingValidator #-}
-- This should validate if either beneficiary1 has signed the transaction and the current slot is before or at the deadline
-- or if beneficiary2 has signed the transaction and the deadline has passed.
mkVestingValidator :: VestingDatum -> () -> ScriptContext -> Bool
mkVestingValidator dat () ctx =
    (beneficiary1HasSigned && isBeforeDeadline) || (beneficiary2HasSigned && isAfterDeadline)
  where
    info = scriptContextTxInfo ctx
    beneficiary1HasSigned = txSignedBy info (beneficiary1 dat)
    beneficiary2HasSigned = txSignedBy info (beneficiary2 dat)
    isBeforeDeadline = contains (to $ deadline dat) $ txInfoValidRange info
    lowerPosixTime = ()
    isAfterDeadline = before (deadline dat) (txInfoValidRange info) 

{-# INLINABLE  mkWrappedVestingValidator #-}
mkWrappedVestingValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedVestingValidator = wrapValidator mkVestingValidator

validator :: Validator
validator = mkValidatorScript $$(compile [|| mkWrappedVestingValidator ||])
