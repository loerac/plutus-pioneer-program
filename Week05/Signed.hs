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

module Week05.Signed where

import           Control.Monad          hiding (fmap)
import           Data.Aeson             (ToJSON, FromJSON)
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
import           Playground.Contract    (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH          (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types       (KnownCurrency (..))
import           Prelude                (IO, Show (..), String)
import           Text.Printf            (printf)
import           Wallet.Emulator.Wallet

{-
 - Mint and/or Burn tokens of `CurrencySymbol` requires a `TokenName` and
 - `Integer`. This is added in our `SignedSchema` to only accept a data type of
 - that matter.
 -
 - Here a new endpoint is created, "mint", that stores the `CurrencySymbol`
 - (`TokenName`, `Amount`). If the `Amount` is a positive number, it'll mint
 - new tokens. If the `Amount` is negative, it'll burn them.
 -}
data MintParams = MintParams {
    mpTokenName :: !TokenName,
    mpAmount    :: !Integer
} deriving (Generic, ToJSON, FromJSON, ToSchema)

type SignedSchema = Endpoint "mint" MintParams

{-
 - @brief:  Similar to the [Free.hs](Free.sh) `mkPolicy` except each policy in
 -          the script context is signed
 - @param:  pkh - Contains the hash of the transaction that was signed
 - @param:  ctx - Contains a list of all the signatories of the transaction
 - @return: True if the `pkh` is in the `ctx` list and signed, else False
 -
 - Similar to the `mkValidator`, the `mkPolicy` needs to be compiled into
 - Plutus Core buy using `mkMonetaryPolicyScript` and inlining our `mkPolicy`
 -
 - `txSignedBy` takes a transaction (`scriptContextTxInfo ctx`) and a public
 - key hash (`pkh`), then return a boolean based on the result of a hash is
 - found in transaction.
 -}
{-# INLINABLE mkPolicy #-}
mkPolicy :: PubKeyHash -> () -> ScriptContext -> Bool
mkPolicy pkh () ctx = txSignedBy (scriptContextTxInfo ctx) pkh

policy :: PubKeyHash -> Scripts.MintingPolicy
policy pkh = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy . mkPolicy ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode pkh

curSymbol :: PubKeyHash -> CurrencySymbol
curSymbol = scriptCurrencySymbol . policy

{-
 - @brief:  Mint a new token with a certain amount
 - @param:  mp - Data struct of the MintParams that contains the token name and
 -               amount
 - @return: Contract monad
 -          w -> Not used (written in lowercase to indicate that)
 -          FreeSchema -> Blockchain's and access to the endpoint(s) ("mint" endpoint)
 -          Text -> Error message returned
 -          () -> Void / Unit to return nothing
 -
 - Firstly, get the public key hash of the contract using `pubKeyHash` function
 - with `Contract.ownPubKey`. Using the `<$>` operator is a synonym for `fmap`.
 -
 - Secondly, create a `Value` using the `Value.singleton` function. The `Value`
 - is stored in `val` which is a map of the `curSymbol` of the policy,
 - `mpTokenName` (from `MintParams`), and `mpAmount` (from `MintParams`).
 -
 - Thirdly, given the policy, look up the policy to create a transaction. This
 - is used to add additional information to the transaction validation.
 -
 - Fourthly, create a transaction, `tx`, with the `Constraints.mustForgeValue`
 - and `val`, which will give a transaction that can now be sent to the chain
 - via `submitTxConstraintsWith`. If not enough funds are given to `val`,
 - `submitTxConstraintsWith` will fail.
 -
 - Fifthly, wait (`awaitTxConfirmed`) for the transaction (`ledgerTx`) to
 - confirm.
 -
 - Lastly, log how many tokens were forged.
 -}
mint :: MintParams -> Contract w SignedSchema Text ()
mint mp = do
    pkh <- pubKeyHash <$> Contract.ownPubKey
    let val     = Value.singleton (curSymbol pkh) (mpTokenName mp) (mpAmount mp)
        lookups = Constraints.mintingPolicy $ policy pkh
        tx      = Constraints.mustMintValue val
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ txId ledgerTx
    Contract.logInfo @String $ printf "forged %s" (show val)

endpoints :: Contract () SignedSchema Text ()
endpoints = mint' >> endpoints
    where
    mint' = endpoint @"mint" >>= mint

mkSchemaDefinitions ''SignedSchema

mkKnownCurrencies []

{-
 - Testing out the functionality of minting and burning the token "ABC". Start
 - off by creating two wallets, wellet 1 and 2, and let each wallet mint 555
 - and 444 tokens of "ABC" respectively. Then have wallet 1 burn -222 tokens of
 - "ABC", this is done by making the amount a negative.
 -
 - In the end, wallet 1 has minted 333 "ABC" tokens, and wallet 2 has minted
 - 444 "ABC" tokens.
 -}
test :: IO ()
test = runEmulatorTraceIO $ do
    let tn = "ABC"
    h1 <- activateContractWallet (Wallet 1) endpoints
    h2 <- activateContractWallet (Wallet 2) endpoints

    callEndpoint @"mint" h1 $ MintParam {
        mpTokenName = tn,
        mpAmount    = 555
    }
    callEndpoint @"mint" h2 $ MintParam {
        mpTokenName = tn,
        mpAmount    = 444
    }

    void $ Emulator.waitNSlots 1

    callEndpoint @"mint" h1 $ MintParam {
        mpTokenName = tn,
        mpAmount    = -222
    }

    void $ Emulator.waitNSlots 1
