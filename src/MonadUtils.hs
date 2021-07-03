module MonadUtils where

import MaybeWithError

(>>=?) :: Monad m => m (Maybe a) -> m b -> (a -> m b) -> m b
(>>=?) m f c = m >>= maybe f c
infixl 1 >>=?

(>>=<>) :: Monad m => m (Either e r) -> (e -> m f) -> (r -> m f) -> m f
(>>=<>) m onleft onright = m >>= either onleft onright
infixl 1 >>=<>

(>>=??) :: Monad m => m (MaybeWithError a e) -> m b -> (a -> m b) -> m b
(>>=??) m f c = m >>= maybe f c . to_maybe
infixl 1 >>=??

(>>=??>) :: Monad m => m (MaybeWithError a e) -> (a -> m (MaybeWithError b e)) -> m (MaybeWithError b e)
(>>=??>) m c =
    m >>= \case
        JustWithError res -> c res
        NothingWithError promise -> return $ NothingWithError promise
infixl 1 >>=??>
