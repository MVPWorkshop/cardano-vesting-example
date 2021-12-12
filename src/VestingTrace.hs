{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds        #-}

module VestingTrace where

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
test = runEmulatorTraceIO runMyTrace

runMyTrace :: EmulatorTrace ()
runMyTrace = do
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

giveAndGrab :: Wallet -> Wallet-> EmulatorTrace ()
giveAndGrab wallet1 wallet2 = do
    h1 <- activateContractWallet wallet1 (endpoints @ContractError)
    h2 <- activateContractWallet wallet2 (endpoints @ContractError)
    callEndpoint @"give" h1 $ GiveParams
        { gpBeneficiary = walletPubKeyHash w2
        , gpDeadline    = slotToBeginPOSIXTime def 20
        , gpAmount      = 10000000
        }
    void $ waitUntilSlot 20
    callEndpoint @"grab" h2 ()
    s <- waitNSlots 1
    Extras.logInfo $ "reached " ++ show s 


grabWithoutGive :: EmulatorTrace ()
grabWithoutGive = do
    h1 <- activateContractWallet w1 (endpoints @ContractError)
    callEndpoint @"grab" h1 ()
    s <- waitNSlots 1
    Extras.logInfo $ "reached " ++ show s