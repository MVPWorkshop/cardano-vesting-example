{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE LambdaCase #-}


module Vesting where

import           Control.Monad        hiding (fmap)
import           Data.Aeson           (ToJSON, FromJSON)
import           Data.Map             as Map
import           Data.Text            (Text)
import           Data.Void            (Void)
import           GHC.Generics         (Generic)
import           Plutus.Contract
import           PlutusTx             (Data (..))
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), unless)
import           Ledger               hiding (singleton)
import           Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Ada           as Ada
import           Playground.Contract  (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema, AsContractError)
import           Playground.TH        (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types     (KnownCurrency (..))
import           Prelude              (IO, Semigroup (..), Show (..), String)
import           Text.Printf          (printf)
import Ledger.Constraints.OffChain (ownPubKeyHash)
import Data.String ( IsString )
import qualified GHC.Base as Haskell


data VestingDatum = VestingDatum
    { beneficiary :: PubKeyHash
    , deadline    :: POSIXTime
    } deriving Show

PlutusTx.unstableMakeIsData ''VestingDatum

{-# INLINABLE mkValidator #-}
mkValidator :: VestingDatum -> () -> ScriptContext -> Bool
mkValidator dat () ctx = traceIfFalse "beneficiary's signature missing" signedByBeneficiary &&
                         traceIfFalse "deadline not reached" deadlineReached
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    signedByBeneficiary :: Bool
    signedByBeneficiary = txSignedBy info $ beneficiary dat

    deadlineReached :: Bool
    deadlineReached = contains (from $ deadline dat) $ txInfoValidRange info

data Vesting
instance Scripts.ValidatorTypes Vesting where
    type instance DatumType Vesting = VestingDatum
    type instance RedeemerType Vesting = ()

typedValidator :: Scripts.TypedValidator Vesting
typedValidator = Scripts.mkTypedValidator @Vesting
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @VestingDatum @()

validator :: Validator
validator = Scripts.validatorScript typedValidator

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash typedValidator

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator 

data GiveParams = GiveParams
    { gpBeneficiary :: !PubKeyHash
    , gpDeadline    :: !POSIXTime
    , gpAmount      :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

type VestingSchema =
            Endpoint "give" GiveParams
        .\/ Endpoint "grab" ()

give :: AsContractError e => Promise w VestingSchema e ()
give = endpoint @"give" @GiveParams $ \(GiveParams b d a ) -> do
    let dat = VestingDatum
                { beneficiary = b
                , deadline    = d
                }
        tx  = mustPayToTheScript dat $ Ada.lovelaceValueOf a
    void $ submitTxConstraints typedValidator tx
    logInfo @String $ printf "made a gift of %d lovelace to %s with deadline %s" a (show b) (show d) 


grab :: (AsContractError e, IsString e) => Promise w VestingSchema e ()
grab = endpoint @"grab" $ \_ -> do
    now   <- currentTime
    pkh   <- Plutus.Contract.ownPubKeyHash
    utxos <- utxosAt scrAddress
    (oref, o, datum) <- findTheDatum utxos pkh now
    logInfo @String $ printf "datum %s" (show datum) 
    logInfo @String $ printf "oref %s"  (show oref) 
    let lookups = Constraints.unspentOutputs utxos  <>
                  Constraints.otherScript validator
        tx :: TxConstraints Void Void
        tx = mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData ()) <> 
             mustValidateIn (from now)
    void $ submitTxConstraintsWith @Void lookups tx
    -- void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ "collected gifts"
    

endpoints :: (AsContractError e, IsString e) => Contract () VestingSchema e ()
endpoints = do
    logInfo @Haskell.String "Waiting for give or grab endpoint..."
    selectList [give, grab] >> endpoints

-- -- | Try to find the datum at script address, tx to https://discord.com/channels/826816523368005654/826829805387120690/898105562003300363
findTheDatum :: (AsContractError e, IsString e) => Map TxOutRef ChainIndexTxOut -> PubKeyHash -> POSIXTime -> Contract w s e (TxOutRef, ChainIndexTxOut, PlutusTx.BuiltinData )
findTheDatum utxos pkh now =
  searchForDatum (Map.toList utxos)
  where
    searchForDatum []                 = throwError "Empty utxo map or no VestingDatum found!"
    searchForDatum ((oref, o):utxos') = do
      d <- maybeGetDatum o
      case d of
        Nothing     -> searchForDatum utxos'
        Just (Datum datum)  -> case PlutusTx.fromBuiltinData datum of
            Nothing -> searchForDatum utxos'
            Just e  -> if beneficiary e == pkh && deadline e <= now 
                        then return (oref, o, datum)
                       else searchForDatum utxos'

maybeGetDatum :: (AsContractError e, IsString e) => ChainIndexTxOut -> Contract w s e (Maybe Datum)
maybeGetDatum cITxO =
  case cITxO of
    PublicKeyChainIndexTxOut {}                        -> return Nothing
    ScriptChainIndexTxOut    { _ciTxOutDatum=txDatum } -> do
      (Datum e) <- either getDatumFromHash return txDatum
      return $ PlutusTx.fromBuiltinData e
        
getDatumFromHash :: (AsContractError e, IsString e) => DatumHash -> Contract w s e Datum
getDatumFromHash dh =
  datumFromHash dh >>= \case
    Nothing -> throwError "datum not found"
    Just d  -> return d