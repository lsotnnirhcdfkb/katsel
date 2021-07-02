module MonadUtils where

(>>=?) :: Monad m => m (Maybe a) -> m b -> (a -> m b) -> m b
(>>=?) m f c = m >>= maybe f c
infixl 1 >>=?

(>>=<>) :: Monad m => m (Either e r) -> (e -> m f) -> (r -> m f) -> m f
(>>=<>) m onleft onright = m >>= either onleft onright
infixl 1 >>=<>
