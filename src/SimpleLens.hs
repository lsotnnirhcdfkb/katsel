module SimpleLens
    ( Lens(..)

    , view
    , over
    , modify

    , join_lenses
    ) where

data Lens a b = Lens (a -> b) (a -> b -> a)

view :: Lens a b -> a -> b
view (Lens getter _) = getter

over :: Lens a b -> (b -> b) -> a -> a
over (Lens getter setter) f a =
    let b = getter a
        b' = f b
    in setter a b'

modify :: Lens a b -> (b -> (c, b)) -> a -> (c, a)
modify (Lens getter setter) f a =
    let b = getter a
        (res, b') = f b
    in (res, setter a b')

join_lenses :: Lens a b -> Lens b c -> Lens a c
join_lenses (Lens getter1 setter1) (Lens getter2 setter2) =
    Lens
        (getter2 . getter1)
        (\ a c ->
            let b = getter1 a
                b' = setter2 b c
            in setter1 a b'
        )
