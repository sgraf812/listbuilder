{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE TypeFamilies #-}

module Data.ListBuilder.Lazy
  ( ListBuilder
  , empty, singleton, append, cons, snoc
  , fromList, toList
  ) where

import qualified Data.Foldable
import qualified GHC.Exts

data ListBuilder a
  = Cons a !(ListBuilder a)
  | Snoc !(ListBuilder a) a
  | App !(ListBuilder a) !(ListBuilder a)
  | Single a
  | List ![a]
  | Empty
  deriving (Functor)

empty :: ListBuilder a
empty = Empty

singleton :: a -> ListBuilder a
singleton x = Single x

append :: ListBuilder a -> ListBuilder a -> ListBuilder a
append = (<>)

cons :: a -> ListBuilder a -> ListBuilder a
cons = Cons

snoc :: ListBuilder a -> a -> ListBuilder a
snoc = Snoc

fromList :: [a] -> ListBuilder a
fromList = List

toList :: ListBuilder a -> [a]
toList = go []
  where
    go acc Empty = acc
    go acc (Cons x xs) = x : go acc xs
    go acc (Snoc xs x) = go (x:acc) xs
    go acc (App xs ys) = go (go acc ys) xs
    go acc (Single x)  = x:acc
    go acc (List xs)   = xs ++ acc

instance Show a => Show (ListBuilder a) where
  show = show . toList

instance Semigroup (ListBuilder a) where
  xs <> ys = App xs ys

instance Monoid (ListBuilder a) where
  mempty = Empty

instance GHC.Exts.IsList (ListBuilder a) where
  type Item (ListBuilder a) = a
  fromList = fromList
  toList = toList

-- | Defers to @Applicative []@.
instance Applicative ListBuilder where
  pure = Single
  f <*> a = List (toList f <*> toList a)

-- | Defers to @Monad []@.
instance Monad ListBuilder where
  m >>= f = List (toList m >>= toList . f)

instance Foldable ListBuilder where
  foldr f acc Empty = acc
  foldr f acc (Cons x xs) = f x (foldr f acc xs)
  foldr f acc (Snoc xs x) = foldr f (f x acc) xs
  foldr f acc (App xs ys) = foldr f (foldr f acc ys) xs
  foldr f acc (Single x)  = f x acc
  foldr f acc (List xs)   = foldr f acc xs
  toList = toList

instance Traversable ListBuilder where
  traverse f Empty       = pure Empty
  traverse f (Cons x xs) = Cons <$> f x <*> traverse f xs
  traverse f (Snoc xs x) = Snoc <$> traverse f xs <*> f x
  traverse f (App xs ys) = App <$> traverse f xs <*> traverse f ys
  traverse f (List xs)   = List <$> traverse f xs
