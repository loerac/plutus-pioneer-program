{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module Week04.Homework where

import Data.Aeson            (FromJSON, ToJSON)
import Data.Functor          (void)
import Data.Text             (Text)
import GHC.Generics          (Generic)
import Ledger
import Ledger.Ada            as Ada
import Ledger.Constraints    as Constraints
import Plutus.Contract       as Contract
import Plutus.Trace.Emulator as Emulator

data PayParams = PayParams
    { ppRecipient :: PubKeyHash
    , ppLovelace  :: Integer
    } deriving (Show, Generic, FromJSON, ToJSON)

type PaySchema = Endpoint "pay" PayParams

{-
 - @brief:  Function only is made to "pay" an amount, it calls itself
 -          recursively to "pay" as often as we like. The payment is sent to the
 -          recipient (sent from `PayParams->ppRecipient`) with the value of lovelace
 -          (also from `PayParamas->ppLovelace`)
 - @param:  Contract:
 -          w - Logging with `Writer` but it's a unit so no logging
 -          s - Blockchain specific capabilities (`PaySchema` holds the public
 -              key hash and value
 -          e - Types error message (`Text`)
 -          a - Overal result to computation but it's ignored
 -}
payContract :: Contract () PaySchema Text ()
payContract = do
    pp <- endpoint @"pay"
    let tx = mustPayToPubKey (ppRecipient pp) $ lovelaceValueOf $ ppLovelace pp
    void $ submitTx tx
    payContract

{-
 - @brief: Invoke the pay endpoint of `payContract` on `Wallet 1` twice, each
 -         time with `Wallet 2` as the recipient. The payment given to `Wallet 2` is
 -         given from the paramaters
 - @param: x - First integer payment to `Wallet 2`
 - @param: y - Second integer payment to `Wallet 2`
 -}
payTrace :: Integer -> Integer -> EmulatorTrace ()
payTrace x y = do
    h <- activateContractWallet (Wallet 1)
    callEndpoint @"pay" h $ PayParams {
        ppRecipient = pubKeyHash $ walletPubKey $ Wallet 2,
        ppLovelace  = x
    }
    void $ Contract.waitNSlots 1
    callEndpoint @"pay" h $ PayParams {
        ppRecipient = pubKeyHash $ walletPubKey $ Wallet 2,
        ppLovelace  = y
    }
    void $ Contract.waitNSlots 1

payTest1 :: IO ()
payTest1 = runEmulatorTraceIO $ payTrace 1000000 2000000

payTest2 :: IO ()
payTest2 = runEmulatorTraceIO $ payTrace 1000000000 2000000
