module Week04.Monad where

{-
 - The definition of a Monda is
 --------
 -- Monad m where
 --     (>>=)  :: m a -> (a -> m b) -> m b
 --     (>>)   :: m a ->  m b -> m b
 --     return :: a -> m a
 --------
 -
 - The `(>>=)` is the same as the `bindMaybe`, `bindEither`, `bindWriter` functions:
 --------
 -- (>>=)      :: IO a            -> (a -> IO b)            -> IO b
 -- bindMaybe  :: Maybe a         -> (a -> Maybe b)         -> Maybe b
 -- bindEither :: Either String a -> (a -> Either String b) -> Either String b
 -- bindWriter :: Writer a        -> (a -> Writer b)        -> Writer b
 --
 -- return              :: a -> IO a
 -- Just                :: a -> Maybe a
 -- Right               :: a -> Either String a
 -- (\a -> Writer a []) :: a -> Writer a
 --------
 -
 - The `(>>)` is the same bind operator but it doesn't care about the output,
 - it ignores it.
 -
 - The `return` takes a `a` type and turns it to a `m a` type.
 -}

threeInts :: Monad m => m Int -> m Int -> m Int -> m Int
threeInts mx my mz =
    mx >>= \k ->
    my >>= \l ->
    mz >>= \m ->
    let s = k + l + m in return s

threeInts' :: Monad m => m Int -> m Int -> m Int -> m Int
threeInts' mx my mz = do
    k <- mx
    l <- my
    m <- mz
    let s = k + l + m
    return s
