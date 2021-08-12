# Week 04

Video: [Iteration #2 Lecture #4](https://www.youtube.com/watch?v=g4lvA14I-Jg)

====

### Monad
Start off with learning [monads](http://www.learnyouahaskell.com/a-fistful-of-monads) (a beefed up applicative functors).
To get a better understanding with monads, we'll work on some examples that use monads
* [Maybe](http://learnyouahaskell.com/a-fistful-of-monads#getting-our-feet-wet-with-maybe) - [Maybe example](Maybe.hs)
* [Either](http://learnyouahaskell.com/for-a-few-monads-more#error) - [Either example](Either.hs)

Monads are used in the wallets and off-chain code, these are called Contract
Monad. They are a concept of computation. It has four parameters:
```hs
-- Contract w s e a
```
* `writer`  `w`:  Allows you to write a log message of type `w` (`Writer`). This can be
seen in the [`Writer.hs`](Writer.hs) example that was done
* `schema`  `s`:  Describes the blockchain capabilities, what contract specific actions
this contract can perform (i.e. waiting for slots, submiting transactions,
finding out public key or specific endpoints)
* `error`   `e`:  Error messages
* `answer`  `a`:  Overall result of the computation


Using the examples, here is the main idea of Monads.
```hs
-1- (>>=)	:: IO a		    -> (a -> IO b)		-> IO b
-2- bindMaybe	:: Maybe a	    -> (a -> Maybe b)		-> Maybe b
-3- bindEither	:: Either string a  -> (a -> Either String b)	-> Either String b
-4- bindWriter	:: Writer a         -> (a -> Writer b)		-> Writer b

-5- return		:: a -> IO a
-6- Just		:: a -> Maybe a
-7- Right		:: a -> Either String a
-8- (\a -> Writer a[])	:: a -> Writer a
```

Each type constructor has one type parameter
1. Used for real world side effects
2. Used for computations that can fail
3. Used for computations that can fail with an error message
4. USed for cumputations that can produces list of string as log output

How the bind works, it depends on the case.
1. It is built in magic that combine the two plans that describe the IO actions
to take during execution
2. The logic is that if the first computation fails, then the combination also
fails; but if the first computation succeeds, then continue.
3. The logic is that if the first computation fails, then the combination also
fails; but if the first computation succeeds, then continue.
4. Combines the lists of log messages from the two parts of the computation.

What are the side-effects that are produces with these computations?
5. Given an `a`, it'll return an `IO a`, the computation returns an `a` and has
no side-effects
6. Given an `a`, it'll check the case of `a`; `Nothing` or `Just a`. If it's
`Nothing`, it'll return `Nothing` and won't fail. If it's `Just a`, it'll
return the computation of `a`. So maybe `a` or nothing.
7. Given an `a`, it'll return the `Left` (string error message) or `Right`
(computation) constructor.
8. Given an `a`, it'll produces a `Writer a` with an empty list of log
messages. It could also be used as given an `a`, it can produces a log message
producing computation of type `a` that doesn't make use of this possibility to
produce log messages and simply doesn't log and immediately returns the
results.

The combination of these two features, to bind two computations together and
construct a computation from a pure value without making use of any of the
potential side effects is what defines a monad.

### `EmulatorTrace` Monad
This is a better way of testing contracts without copying/pasting code to the
plutus playground.
```hs
runEmulatorTrace
    :: EmulatorConfig
    -> EmulatorTrace ()
    -> ([EmulatorEvent], Maybe Emulator, EmulatorState)
```

This executes a trace on an emulated blockchain. It expects an
`EmulatorConfig`, initial state of the blockchain (`Value` in each wallet at
the start of trace execution.

An example of a `"give"` and `"grab"` (just like the last few weeks).
1. Create two wallets (`Wallet 1` and `Wallet 2`), and store them in a handle
(`h1` and `h2` respectively).
2. Set up an endpoint where Wallet 1 will give 1000 Lovelace to Wallet 2 after
the 20th slot
3. Wait until the 20th slot
4. Set up endpoint where Wallet 2 will grab the 1000 Lovelace
5. Wait for 1 slot and then log the computation
```hs
myTrace :: EmulatorTrace ()
myTrace  = do
    h1 <- activateContractWallet (Wallet 1) endpoints
    h2 <- activateContractWallet (Wallet 2) endpoints
    callEndpoint @"give" h1 $ GiveParams {
        gpBeneficiary   = pubKeyHash $ walletPubKey $ Wallet 2,
        gpDeadline      = Slot 20,
        gpAmount        = 1000
    }
    void $ waitUntilSlot 20
    callEndpoint @"grab" h2 ()
    s <- waitNSlots 1
    Extras.logInfo $ "reached slot " ++ show s
```
