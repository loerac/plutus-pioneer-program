module Week04.Maybe where

import Text.Read (readMaybe)

{-
 - @brief:  Should read all three parameter strings as ints. If successful,
 -          it'll return it as an int, else nothing. In this example, the
 -          pattern is repeated three times (which isn't the best)
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
 -          The bind is done buy first checking if the variable is `Nothing`,
 -          if it is, then the whole computation is `Nothing`. Else, if the
 -          variable is `Just`, then continue with the value of the `Just`
 -
 -          Think of this as `try and except` clauses in other languages; if an
 -          exception is thrown, the whole computation is stopped.
 -}
bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybe Nothing  _ = Nothing
bindMaybe (Just x) f = f x

{-
 - @brief:  Each string input is put through the bind which checks if it's
 -          `Nothing` or `Just`. It'll add x, y, and z all together as long as
 -          none of them are `Nothing`.
 -
 -          First, read the value of `x` (`Maybe a ->`). Then bind that to
 -          `bindMaybe` (`a -> Maybe b`). Afterwards, if the computation
 -          succeed (`readMaybe x` is not `Nothing`), store it in `\x'` (`Maybe
 -          b`). This continues on for `y` and `z`.
 -}
foo2 :: String -> String -> String -> Maybe Int
foo2 x y z = readMaybe x `bindMaybe` \x' ->
             readMaybe y `bindMaybe` \y' ->
             readMaybe z `bindMaybe` \z' ->
             Just (x' + y' + z')
