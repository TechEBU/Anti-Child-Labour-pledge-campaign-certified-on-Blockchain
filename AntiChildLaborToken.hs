{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module AntiChildLaborToken where

import           Control.Monad          hiding (fmap)
import           Data.Aeson             (FromJSON, ToJSON)
import qualified Data.Map               as Map
import           Data.Text              (Text)
import           Data.Void              (Void)
import           GHC.Generics           (Generic)
import           Plutus.Contract        as Contract
import           Plutus.Trace.Emulator  as Emulator
import qualified PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Ledger                 hiding (mint, singleton)
import           Ledger.Constraints     as Constraints
import qualified Ledger.Typed.Scripts   as Scripts
import           Ledger.Value           as Value
import           Prelude                (IO, Semigroup (..), Show (..), String)
import           Text.Printf            (printf)
import           Wallet.Emulator.Wallet

{-  When the third-party auditor evaluates the supplier's compliance with the monthly objectives of the
AntiChildLabour campaign and issues a favorable rating, the Company leading the campaign proceeds to minting an NFT
and deposit it into the supplier's authorized wallet. -}


{-# INLINABLE mkPolicy #-}
mkPolicy :: TxOutRef -> TokenName -> () -> ScriptContext -> Bool
mkPolicy oref tc () ctx = traceIfFalse "UTxO not consumed"   hasUTxO           &&
                          traceIfFalse "wrong amount minted" checkMintedAmount
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    hasUTxO :: Bool
    hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

    checkMintedAmount :: Bool
    checkMintedAmount = case flattenValue (txInfoMint info) of
        [(_, tc', amt)] -> tc' == tc && amt == 1
        _               -> False

policy :: TxOutRef -> TokenName -> Scripts.MintingPolicy
policy oref tc = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \oref' tc' -> Scripts.wrapMintingPolicy $ mkPolicy oref' tc' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode oref
    `PlutusTx.applyCode`
    PlutusTx.liftCode tc

curSymbol :: TxOutRef -> TokenName -> CurrencySymbol
curSymbol oref tc = scriptCurrencySymbol $ policy oref tc



data NFTParams = NFTParams
    { npToken   :: !TokenName
    , npAddress :: !Address
    } deriving (Generic, FromJSON, ToJSON, Show)

type NFTSchema = Endpoint "mint" NFTParams

mint :: NFTParams -> Contract w NFTSchema Text ()
mint np = do
    utxos <- utxosAt $ npAddress np
    case Map.keys utxos of
        []       -> Contract.logError @String "no utxo found"
        oref : _ -> do
            let tc      = npToken np
            let val     = Value.singleton (curSymbol oref tc) tc 1
                lookups = Constraints.mintingPolicy (policy oref tc) <> Constraints.unspentOutputs utxos
                tx      = Constraints.mustMintValue val <> Constraints.mustSpendPubKeyOutput oref
            ledgerTx <- submitTxConstraintsWith @Void lookups tx
            void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
            Contract.logInfo @String $ printf "forged %s" (show val)

endpoints :: Contract () NFTSchema Text ()
endpoints = mint' >> endpoints
  where
    mint' = awaitPromise $ endpoint @"mint" mint



{- In the field "tc" the Company introduces the serial number that will be assigned to the NFT.  
The serial number of the NFT will be an Integer. -}

test :: IO ()
test = runEmulatorTraceIO $ do
    let tc = "1122334455"
        w1 = knownWallet 1
       
    h1 <- activateContractWallet w1 endpoints
   
    callEndpoint @"mint" h1 $ NFTParams
        { npToken   = tc
        , npAddress = mockWalletAddress w1
        }
   
    void $ Emulator.waitNSlots 1
    