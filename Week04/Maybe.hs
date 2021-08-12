module Week04.Maybe where

import Text.Read (readMaybe)
import Week04.Monad

{-
 - @brief:  Should read all three parameter strings as ints. If successful,
 -          it'll return it as an int, else nothing. In this example, the
 -          pattern is repeated three times (which isn't the best)
 -
 -          The function does 3 cases, each case tries to compute the input
 -          variable with the `readMaybe` haskell function. `readMaybe` checks
 -          if a string is an `Integer`; if it is, return `Just Int`, otherwise
 -          `Nothing`.
 -
 -          When the case for `Nothing` occurs, it'll break out of the cases
 -          and return `Nothing` for `foo1` function. Else, it'll keep moving
 -          down to the next case and return the computation.
 -}
foo1 :: String -> String -> String -> Maybe Int
foo1 x y z = case readMaybe x of
    Nothing -> Nothing
    Just x' -> case readMaybe y of
        Nothing -> Nothing
        Just y' -> case readMaybe z of
            Nothing -> Nothing
            Just z' -> Just (x' + y' + z')

{-
 - @brief:  Clean up `foo1` to not repeat the pattern three times. This is done
 -          by binding each `readMaybe` together, and if one of the binds is
 -          `Nothing`, then return `Nothing` all together.
 -
 -          Check if the first parameter is `Nothing` (ignoring the inline
 -          function), if so, then the whole computation is `Nothing`. Else,
 -          continue with the inline function.
 -
 -          Think of this as `try and except` clauses in other languages; if an
 -          exception is thrown, the whole computation is stopped.
 - @param:  `Maybe a`, which is either `Nothing` or `Just`
 - @param:  `(a -> Maybe b)` is an inline function
 -}
bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybe Nothing  _ = Nothing
bindMaybe (Just x) f = f x

{-
 - @brief:  Each string input is put through the bind which checks if it's
 -          `Nothing` or `Just`. It'll add x, y, and z all together as long as
 -          none of them are `Nothing`.
 -
 -          Execute `bindMaybe` with the output of `readMaybe x` and the
 -          anonymous function `\k`. When the output of `readMaybe` is
 -          `Nothing`, return `Nothing` and exit. Else, continue to the next
 -          variable. At the end, all the variables are type `Just Int` so
 -          compute the computation of the input.
 -}
foo2 :: String -> String -> String -> Maybe Int
foo2 x y z = readMaybe x `bindMaybe` \k ->
             readMaybe y `bindMaybe` \l ->
             readMaybe z `bindMaybe` \m ->
             Just (k + l + m)

foo3 :: String -> String -> String -> Maybe Int
foo3 x y z = threeInts (readMaybe x) (readMaybe y) (readMaybe z)
