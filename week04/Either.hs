module Week04.Either where

import Text.Read (readMaybe)

{-
 - @brief:  Using either, check if either the variable `a` can be parsed.
 -          If the variable can be parsed, return `Right`.
 -          Else, return, `Left', an error message
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
 -          The bind is done buy first checking if the variable is `Left`,
 -          if it is, then the whole computation is stopped and an error message
 -          is returned.. Else, if the variable is `Right`, then continue with
 -          the value of the `Right`
 -
 -          Think of this as `try and except` clauses in other languages; if an
 -          exception is thrown, the whole computation is stopped.
 -}
bindEither :: Either String a -> (a -> Either String b) -> Either String b
bindEither (Left err) _ = Left err
bindEither (Right x)  f = f x

{-
 - @brief:  Each string input is put through the bind which checks if it's
 -          `Left` or `Right`. It'll add x, y, and z all together as long as
 -          none of them are `Left`
 -
 -          First, read the value of `x` (`Either String a ->`). Then bind that
 -          to `bindEither` (`a -> Either String b`). Afterwards, if the
 -          computation succeed (`readEither x` is not `Left`), store it in
 -          `\x'` (`Either String b`). This continues on for `y` and `z`.
 -}
foo2 :: String -> String -> String -> Either String Int
foo2 x y z = readEither x `bindEither` \x' ->
             readEither y `bindEither` \y' ->
             readEither z `bindEither` \z' ->
             Right (x' + y' + z')
