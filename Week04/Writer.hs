module Week04.Writer where

import Control.Monad
import Week04.Monad

{-
 - Custom data type which takes a variable and a list of strings. `deriving
 - Show` makes sure it can output it's content.
 -}
data Writer a = Writer a [String]
    deriving Show

{-
 - @brief:  Takes an `Integer` and returns a `Writer` with that `Integer` and a
 -          list with one time in it saying "number: <NUMBER>".
 - @param:  n - Integer that is to be used as a `Writer`
 -}
number :: Int -> Writer Int
number n = Writer n $ ["number: " ++ show n]

{-
 - Takes a list with strings and creates a `Writer` with a type Unit for our
 - `Integer` (void) and the list of strings as the second parameter
 -}
tell :: [String] -> Writer ()
tell = Writer ()

{-
 - @brief:  Adds up all the integer values and creates an array with the
 -          integers and sum of the integers.
 -
 -          Take 3 instances of `Writer` and combines all the values to a final
 -          `Writer` instance. First, compute the sum of all 3 `Writers` and
 -          store in in `s`. Secondly, call `tell` to give a list of strings
 -          with the sum content and storing it in `us`. Lastly, return a new
 -          `Writer` instance with the sum and list of all the list strings as
 -          one
 -}
foo1 :: Writer Int -> Writer Int -> Writer Int -> Writer Int
foo1 (Writer x xs) (Writer y ys) (Writer z zs) =
    let
        s = x + y + z
        Writer _ us = tell ["sum: " ++ show s]
    in
        Writer s $ xs ++ ys ++ zs ++ us

{-
 - @brief:  Similar to `foo1` above but instead of appending all the arrays
 -          together, it'll bind each parameter together.
 -
 -          Given the `Writer` instance, it'll continuely go down all the
 -          variables until the inline function, `f`, is empty, `\_` or `\()`.
 -          Once the end is reached, it'll append each list of strings together
 -          (starting from the end to the beginning i.e. `[]` -> ... -> `[x]`
 -          with the new instance of `Writer b`.
 -}
bindWriter :: Writer a -> (a -> Writer b) -> Writer b
bindWriter (Writer a as) f =
    let
        Writer b bs = f a
    in
        Writer b $ as ++ bs

{-
 - @brief:  Each `Writer` iput is binded with the next `Writer` instance until
 -          the end of the bind is reached (`\_`). Once there, a new `Writer` instance
 -          is outputed with the sum of the `Writer` values and an log message.
 -
 -          Execute `bindWriter` with the input of `x` and the anonymous
 -          function `\k`. When the input of the anonymous function is empty,
 -          return the new instance of `Writer` with all the `Writer`s summed
 -          together with a log message.
 -}
foo2 :: Writer Int -> Writer Int -> Writer Int -> Writer Int
foo2 x y z = x `bindWriter` \k ->
             y `bindWriter` \l ->
             z `bindWriter` \m ->
             let s = k + l + m
             in tell ["sum: " ++ show s] `bindWriter` \_ ->
                Writer s []

{-
 - @brief:  Similar to the other two foo functions but this time using a built
 -          in bind function that will bind all three `Writers` and tally the sum with a
 -          log message.
 -}
foo3 :: Writer Int -> Writer Int -> Writer Int -> Writer Int
foo3 x y z = do
    s <- threeInts x y z
    tell ["sum: " ++ show s]
    return s

instance Functor Writer where
    fmap = liftM

instance Applicative Writer where
    pure  = return
    (<*>) = ap

instance Monad Writer where
    return a = Writer a []
    (>>=) = bindWriter
