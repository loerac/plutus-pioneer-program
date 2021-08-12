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

module Week05.NFT where

import           Control.Monad          hiding (fmap)
import qualified Data.Map               as Map
import           Data.Text              (Text)
import           Data.Void              (Void)
import           Plutus.Contract        as Contract
import           Plutus.Trace.Emulator  as Emulator
import qualified PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Ledger                 hiding (mint, singleton)
import           Ledger.Constraints     as Constraints
import qualified Ledger.Typed.Scripts   as Scripts
import           Ledger.Value           as Value
import           Playground.Contract    (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH          (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types       (KnownCurrency (..))
import           Prelude                (IO, Semigroup (..), Show (..), String)
import           Text.Printf            (printf)
import           Wallet.Emulator.Wallet

{-
 - Here a new endpoint is created, "mint", that stores only the Token name
 - of the NFT that is created.
 -}
type NFTSchema = Endpoint "mint" TokenName

{-
 - @brief:  Similar to the [Free.hs](Free.sh) `mkPolicy` except each policy in
 - @param:  oref - Identifire for the output reference of the transaction
 - @param:  tn - Token name of the currency symbol
 - @param:  ctx - Contains a list of all the signatories of the transaction
 - @return: True if the `hasUTxO` and `checkMintedAmount` are valid, else False
 -
 - Similar to the `mkValidator`, the `mkPolicy` needs to be compiled into
 - Plutus Core buy using `mkMonetaryPolicyScript` and inlining our `mkPolicy`
 -}
{-# INLINABLE mkPolicy #-}
mkPolicy :: TxOutRef -> TokenName -> () -> ScriptContext -> Bool
mkPolicy oref tn () ctx =   traceIfFalse "UTxO not consumed" hasUTxO &&
                            traceIfFalse "Wrong amount minted" checkMintedAmount
    where
        info :: TxInfo
        info = scriptContextTxInfo ctx

        {-
         - Checks if the UTxO (`oref`) is found in the `txInfoInputs`. If not
         - found, an error is thrown, else continue
         -}
        hasUTxO :: Bool
        hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

        {-
         - Compare the values from `txInfoForged info` to the NFT list
         - `(CurrencySymbol, TokenName, Amount)` which should be equal to `(cs,
         - tn, 1)` (where `cs` and `tn` is similar to the policy). Limit the
         - amount of possible minted tokens to one (1) since NFT can only
         - exists once.
         -
         - Use `ownCurrencySymbol` to get access to the `CurrencySymbol` during
         - execution of the Policy Script. This prevents a deadlock from
         - happening.
         -}
        checkMintedAmount :: Bool
        checkMintedAmount = case flattenValue (txInfoForge info ) of
            [(cs, tn', amt)]    -> cs == ownCurrencySymbol ctx && tn' == tn && amt == 1
            _                   -> False

policy :: TxOutRef -> TokenName -> Scripts.MintingPolicy
policy oref tn = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \oref' tn' -> Scripts.wrapMintingPolicy $ mkPolicy oref' tn' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode oref
    `PlutusTx.applyCode`
    PlutusTx.liftCode tn

curSymbol :: TxOutRef -> TokenName -> CurrencySymbol
curSymbol oref tn = scriptCurrencySymbol $ policy oref tn

{-
 - @brief:  Mint a new NFT.
 - @param:  tn - Data struct of the MintParams that contains the token name and
 - @return: Contract monad
 -          w -> Not used (written in lowercase to indicate that)
 -          FreeSchema -> Blockchain's and access to the endpoint(s) ("mint" endpoint)
 -          Text -> Error message returned
 -          () -> Void / Unit to return nothing
 -
 - Firstly, get our public key hash (from the on-chain) of the contract using
 - `pubKeyHash` function with `Contract.ownPubKey`. Using that publick key
 - hash, get an address to a UTxO with the `pubKeyAddress`. Then using this
 - address, look for all valid UTxOs at that address using `utxoAt`, which
 - returns a map of our UTxO.
 -
 - Secondly, get the key from the UTxO and check if the address was found. An
 - empty map (`[]`) will return an error stating that no UTxO was found.
 -
 - Thirdly, create a `Value` using the `Value.singleton` function. The `Value`
 - is stored in `val` which is a map of the `curSymbol` of the policy,
 - `mpTokenName` (from `MintParams`), and `mpAmount` (from `MintParams`).
 -
 - Fourthly, given the policy, look up the policy to create a transaction. This
 - is used to add additional information to the transaction validation. This is
 - combined with the `unspentOutputs` constraint on the `utxos`.
 -
 - Fifthly, create a transaction, `tx`, with the `Constraints.mustForgeValue`
 - and `val`, which will give a transaction that can now be sent to the chain
 - via `submitTxConstraintsWith`. This is combined with the
 - `mustSpendPubKeyOutput` contstraint on the `oref`.  If not enough funds are
 - given to `val`, `submitTxConstraintsWith` will fail.
 -
 - Sixthly, wait (`awaitTxConfirmed`) for the transaction (`ledgerTx`) to
 - confirm.
 -
 - Lastly, log how many tokens were forged.
 -}
mint :: TokenName -> Contract w NFTSchema Text ()
mint tn = do
    pkh <- Contract.ownPubKey
    utxos <- utxoAt (pubKeyAddress pkh)
    case Map.keys utxos of
        []      -> Contract.logError @String "no utxo found"
        oref: _ -> do
            let val     = Value.singleton (curSymbol oref tn) tn 1
                lookups = Constraints.mintingPolicy (policy oref tn) <> Constraints.unspentOutputs utxos
                tx      = Constraints.mustMintValue val <> Constraints.mustSpendPubKeyOutput oref
            ledgerTx <- submitTxConstraintsWith @Void lookups tx
            void $ awaitTxConfirmed $ txId ledgerTx
            Contract.logInfo @String $ printf "forged %s" (show val)


endpoints :: Contract () NFTSchema Text ()
endpoints = mint' >> endpoints
    where
    mint' = endpoint @"mint" >>= mint

mkSchemaDefinitions ''NFTSchema

mkKnownCurrencies []

{-
 - Testing out the functionality of minting and burning the token "ABC". Start
 - off by creating two wallets, wallet 1 and 2, and let each wallet mint one
 - NFT. They only contain a token name and no way to specify the amount
 -}
test :: IO ()
test = runEmulatorTraceIO $ do
    let tn = "ABC"
    h1 <- activateContractWallet (Wallet 1) endpoints
    h2 <- activateContractWallet (Wallet 2) endpoints

    callEndpoint @"mint" h1 tn
    callEndpoint @"mint" h2 tn
    void $ Emulator.waitNSlots 1
