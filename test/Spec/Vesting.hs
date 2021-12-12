{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Spec.Vesting
    ( tests
    ) where

import           Plutus.Trace.Emulator (ContractInstanceTag)
import qualified Plutus.Trace.Emulator as Trace
import           Vesting
import           Plutus.Contract       (Contract, ContractError(WalletError))
import           Test.Tasty ( testGroup, TestTree )
import qualified Test.Tasty.HUnit      as HUnit
import           Plutus.Contract.Test
import           Control.Monad         (void)
import           VestingTrace
import qualified Ledger.Ada            as Ada
import Ledger
import Wallet
import Data.String
import Plutus.Contract.Types
import Data.String (IsString)

t1, t2 :: ContractInstanceTag
t1 = Trace.walletInstanceTag w1
t2 = Trace.walletInstanceTag w2

theContract :: Contract () VestingSchema ContractError ()
theContract = endpoints

tests :: TestTree
tests = testGroup "vesting"
    [
       checkPredicate "Expose 'give' endpoint, and 'grab' endpoint"
        (endpointAvailable @"give" theContract t1
          .&&. endpointAvailable @"grab" theContract t1)
        $ void $ Trace.activateContractWallet w1 theContract 
      ,
      checkPredicate "Give and Grab working" 
      (walletFundsChange w2 (Ada.adaValueOf 10)
          .&&. walletFundsChange w1 (Ada.adaValueOf (-10))) 
      $ giveAndGrab w1 w2
      ,
      checkPredicate "Grab before give should not work" 
      (walletFundsChange w2 (Ada.lovelaceValueOf 0)
          .&&. walletFundsChange w1 (Ada.adaValueOf 0)
          .&&. assertContractError theContract t1 appropriateError "OtherError 'Empty utxo map or no VestingDatum found!'")
      grabWithoutGive
    ]

appropriateError :: ContractError -> Bool
appropriateError e = case e of
    Plutus.Contract.Types.OtherError "Empty utxo map or no VestingDatum found!" -> True
    _ -> False