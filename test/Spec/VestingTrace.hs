{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds        #-}

module Spec.Trace where

import Control.Monad.Freer.Extras as Extras
import Data.Default               (Default (..))
import Data.Functor               (void)
import Ledger
import Ledger.TimeSlot
import Plutus.Trace.Emulator      as Emulator
import Wallet.Emulator.Wallet
import           Plutus.Contract.Test
import Wallet.Types
import Vesting
import Data.Void as Void
import Plutus.Contract (endpoint)

-- Contract w s e a
-- EmulatorTrace a

test :: IO ()
test = runEmulatorTraceIO myTrace

myTrace :: EmulatorTrace ()
myTrace = do
    h1 <- activateContractWallet w1 (endpoints @ContractError)
    h2 <- activateContractWallet w2 (endpoints @ContractError)
    callEndpoint @"give" h1 $ GiveParams
        { gpBeneficiary = walletPubKeyHash w2
        , gpDeadline    = slotToBeginPOSIXTime def 20
        , gpAmount      = 10000000
        }
    void $ waitUntilSlot 20
    callEndpoint @"grab" h2 ()
    s <- waitNSlots 1
    Extras.logInfo $ "reached " ++ show s