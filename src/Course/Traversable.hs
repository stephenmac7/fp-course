{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module Course.Traversable where

import Course.Core
import Course.Functor
import Course.Applicative
import Course.List
import Course.ExactlyOne
import Course.Optional
import Course.Compose

-- | All instances of the `Traversable` type-class must satisfy two laws. These
-- laws are not checked by the compiler. These laws are given as:
--
-- * The law of naturality
--   `∀f g. f . traverse g ≅ traverse (f . g)`
--
-- * The law of identity
--   `∀x. traverse ExactlyOne x ≅ ExactlyOne x`
--
-- * The law of composition
--   `∀f g. traverse ((g <$>) . f) ≅ (traverse g <$>) . traverse f`
class Functor t => Traversable t where
  traverse ::
    Applicative f =>
    (a -> f b)
    -> t a
    -> f (t b)

instance Traversable List where
  traverse ::
    Applicative f =>
    (a -> f b)
    -> List a
    -> f (List b)
  traverse f =
    foldRight (\a b -> (:.) <$> f a <*> b) (pure Nil)

instance Traversable ExactlyOne where
  traverse ::
    Applicative f =>
    (a -> f b)
    -> ExactlyOne a
    -> f (ExactlyOne b)
  traverse f (ExactlyOne x) = ExactlyOne <$> f x

instance Traversable Optional where
  traverse ::
    Applicative f =>
    (a -> f b)
    -> Optional a
    -> f (Optional b)
  traverse _ Empty = pure Empty
  traverse f (Full x) = Full <$> f x

-- | Sequences a traversable value of structures to a structure of a traversable value.
--
-- >>> sequenceA (ExactlyOne 7 :. ExactlyOne 8 :. ExactlyOne 9 :. Nil)
-- ExactlyOne [7,8,9]
--
-- >>> sequenceA (Full (ExactlyOne 7))
-- ExactlyOne (Full 7)
--
-- >>> sequenceA (Full (*10)) 6
-- Full 60
sequenceA ::
  (Applicative f, Traversable t) =>
  t (f a)
  -> f (t a)
sequenceA = traverse id

instance (Traversable f, Traversable g) =>
  Traversable (Compose f g) where
  traverse ::
    Applicative h =>
    (a -> h b)
    -> (Compose f g) a
    -> h ((Compose f g) b)
  traverse f (Compose x) = Compose <$> (sequenceA $ sequenceA . (f <$>) <$> x)

-- | The `Product` data type contains one value from each of the two type constructors.
data Product f g a =
  Product (f a) (g a)

instance (Functor f, Functor g) =>
  Functor (Product f g) where
  (<$>) :: (a -> b) -> (Product f g) a -> (Product f g) b
  f <$> (Product fv gv) = Product (f <$> fv) (f <$> gv)

instance (Traversable f, Traversable g) =>
  Traversable (Product f g) where
  traverse ::
    Applicative h =>
    (a -> h b)
    -> (Product f g) a
    -> h ((Product f g) b)
  traverse f (Product fv gv) = lift2 Product a b
    where a = traverse f fv
          b = traverse f gv

-- | The `Coproduct` data type contains one value from either of the two type constructors.
data Coproduct f g a =
  InL (f a) | InR (g a)

instance (Functor f, Functor g) =>
  Functor (Coproduct f g) where
  f <$> InL fv = InL $ f <$> fv
  f <$> InR gv = InR $ f <$> gv

instance (Traversable f, Traversable g) =>
  Traversable (Coproduct f g) where
  traverse f (InL fv) = lift1 InL (traverse f fv)
  traverse f (InR fv) = lift1 InR (traverse f fv)
