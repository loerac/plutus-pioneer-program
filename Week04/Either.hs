module Week04.Either where

import Text.Read (readMaybe)
import Week04.Monad

{-
 - @brief:  Using either, check if either the variable `a` can be parsed.
 -          If the variable can be parsed, return `Right`.
 -          Else, return, `Left', an error message
 -
 -          This is similar to `Maybe`, but with the difference of `Either` is
 -          able to return an error message, `Left`, where `Nothing` doesn't
 -          return one.
 -}
readEither :: Read a => String -> Either String a
readEither s = case readMaybe s of
    Nothing -> Left $ "Can't parse: " ++ s
    Just a -> Right a

{-
 - @brief:  Should read all three parameters strings as ints. If successful,
 -          it'll return the right side, else return the left side with an
 -          error message. In this example, the pattern  is repeated three
 -          times (which isn't the best)
 -
 -          The function does 3 cases, each case tries to compute the input
 -          variable with the `readEither`. `readEither` checks if a string is
 -          an `Integer`; if it is, returns the `Right` value of `Either`, else
 -          `Left` value of `Either` with an error message.
 -
 -          When the case for `Nothing` occurs, it'll break out of the cases
 -          and return `Nothing` for `foo1` function. Else, it'll keep moving
 -          down to the next case and return the computation.
 -}
foo1 :: String -> String -> String -> Either String Int
foo1 x y z = case readEither x of
    Left err -> Left err
    Right x' -> case readEither y of
        Left err -> Left err
        Right y' -> case readEither z of
            Left err -> Left err
            Right z' -> Right (x' + y' + z')

{-
 - @brief:  Clean up `foo` to not repeat the pattern three times. This is done
 -          by binding each `readEither` together, and if one of the binds is
 -          `Left`, then return the error message.
 -
 -          Check if the first parameter is `Left` (ignoring the inline
 -          function), if so, then the whole computation is `Left` and returns
 -          an error message.. Else, continue with the inline function with the
 -          value of `Right`.
 -
 -          Think of this as `try and except` clauses in other languages; if an
 -          exception is thrown, the whole computation is stopped.
 - @param:  `Either String a`, which is either `Left` or `Right` with an error
 -          message or value respectively
 - @param:  `(a -> Either String b)` is an inline function
 -}
bindEither :: Either String a -> (a -> Either String b) -> Either String b
bindEither (Left err) _ = Left err
bindEither (Right x)  f = f x

{-
 - @brief:  Each string input is put through the bind which checks if it's
 -          `Left` or `Right`. It'll add x, y, and z all together as long as
 -          none of them are `Left`
 -
 -          Execute `bindEither` with the output of `readEither x` and the
 -          anonymous function `\k`. When the output of `readEither` is
 -          `Right`, return an error message and exit. Else, continue to the
 -          next variable. At the end, all the variables are type `Just Int` so
 -          compute the computation of the input.
 -}
foo2 :: String -> String -> String -> Either String Int
foo2 x y z = readEither x `bindEither` \k ->
             readEither y `bindEither` \l ->
             readEither z `bindEither` \m ->
             Right (k + l + m)

{-
 - Using the built in Monad function that was built in `Monad.hs`, it will
 - bind, compute, and create the log message for us in one line.
 -}
foo2 :: String -> String -> String -> Either String Int
foo2 x y z = threeInts (readEither x) (readEither y) (readEither z)
