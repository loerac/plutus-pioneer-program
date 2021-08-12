{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}

module Week04.Contract where

import Control.Monad.Freer.Extras as Extras
import Data.Functor               (void)
import Data.Text                  (Text, unpack)
import Data.Void                  (Void)
import Plutus.Contract            as Contract
import Plutus.Trace.Emulator      as Emulator
import Wallet.Emulator.Wallet

{-
 - Contract takes 4 parameters
 - `w`: Write log messages of type `w`
 - `s`: Endpoints of the contract
 - `e`: Error messages
 - `a`: Result of the computation
 -}
-- Contract w s e a
-- EmulatorTrace a

{-
 - Two functions that will log info to the screen. One of the functions will
 - log a hello message (`myContract1`) while the other one throws an error
 - (`myContract1'`). `myContract1` will run from start to finish while
 - `myContract1'` will stop right when it throws the error
 -}
myContract1 :: Contract () Empty Text ()
myContract1 = Contract.logInfo @String "hello from the contract"

myContract1' :: Contract () Empty Text ()
myContract1' = do
    void $ Contract.throwError "BOOM!"
    Contract.logInfo @String "hello from the contract"

{-
 - Set an trace emulator and test both of the functions out. The rest will be
 - printed on the screen do to the `test1` and `test1'` return of `IO` type.
 -}
myTrace1 :: EmulatorTrace ()
myTrace1 = void $ activateContractWallet (Wallet 1) myContract1

myTrace1' :: EmulatorTrace ()
myTrace1' = void $ activateContractWallet (Wallet 1) myContract1'

test1 :: IO ()
test1 = runEmulatorTraceIO myTrace1

test1' :: IO ()
test1' = runEmulatorTraceIO myTrace1'

{-
 - Create a function that will throw and catch an error from `myContract1'`.
 - Since `myContract1'` is throwing an error, `myContract2` will catch this
 - error and log it to the screen instead of just erroring out.
 -
 - This is done by `myContract2` calling `myContract1'` which throws the error,
 - `handleError` will catch the error and store it in `\err` and will then log
 - the error to the screen as a `String` (`unpack err` will change `err` type
 - of `Text` to a `String`).
 -}
myContract2 :: Contract () Empty Void ()
myContract2 = Contract.handleError
    (\err -> Contract.logError $ "caught: " ++ unpack err)
    myContract1'

myTrace2 :: EmulatorTrace ()
myTrace2 = void $ activateContractWallet (Wallet 1) myContract2

test2 :: IO ()
test2 = runEmulatorTraceIO myTrace2

{-
 - Creating a data type for the `s` type (endpoint of the contract) in
 - `Contract w s e a`. The contract will have a label of "foo" and an `Int`
 - value to the contract. Whenever the contract is going to be utilized, it'll
 - need the label "foo" to use it.
 -}
type MySchema = Endpoint "foo" Int

{-
 - `test3` runs the emulator for `myContract3` through `myTrace3`, which
 - creates an handle for "foo" with the integer 42. Inside of `myContract3`,
 - it'll get the endpoint of "foo" and log the integer, which is 42.
 -}
myContract3 :: Contract () MySchema Text ()
myContract3 = do
    n <- endpoint @ "foo"
    Contract.logInfo n

myTrace3 :: EmulatorTrace ()
myTrace3 = do
    h <- activateContractWallet (Wallet 1) myContract3
    callEndpoint @"foo" h 42

test3 :: IO ()
test3 = runEmulatorTraceIO myTrace3

{-
 - Creating a data type for the `s` type (endpoint of the contract) in
 - `Contract w s e a`. The contract will have a label of "foo" key with a `Int`
 - value to the contract, as well as a "bar" key with a `String` value.
 - Whenever the contract is going to be utilized, it'll need the label "foo" to
 - use it.
 -}
type MySchema' = Endpoint "foo" Int .\/ Endpoint "bar" String

myContract3' :: Contract () MySchema' Text ()
myContract3' = do
    n <- endpoint @ "foo"
    Contract.logInfo n
    s <- endpoint @ "bar"
    Contract.logInfo s

myTrace3' :: EmulatorTrace ()
myTrace3' = do
    h <- activateContractWallet (Wallet 1) myContract3'
    callEndpoint @"foo" h 42
    callEndpoint @"bar" h "the answer"

test3' :: IO ()
test3' = runEmulatorTraceIO myTrace3'

myContract4 :: Contract [Int] Empty Text ()
myContract4 = do
    void $ Contract.waitNSlots 10
    tell [1]
    void $ Contract.waitNSlots 10
    tell [2]
    void $ Contract.waitNSlots 10

myTrace4 :: EmulatorTrace ()
myTrace4 = do
    h <- activateContractWallet (Wallet 1) myContract4

    void $ Emulator.waitNSlots 5
    xs <- observableState h
    Extras.logInfo $ show xs

    void $ Emulator.waitNSlots 10
    ys <- observableState h
    Extras.logInfo $ show ys

    void $ Emulator.waitNSlots 10
    zs <- observableState h
    Extras.logInfo $ show zs

test4 :: IO ()
test4 = runEmulatorTraceIO myTrace4
