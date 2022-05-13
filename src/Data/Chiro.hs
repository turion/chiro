{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
module Data.Chiro where
import Data.Foldable (fold)
import GHC.TypeLits
import Data.Kind (Type)

data Chiro (n :: Nat) (k :: Type) (a :: Type) where
  Empty :: Chiro 0 k a
  Single :: a -> Chiro 1 k a
  Tree ::
    k ->
    (Digit l a) ->
    (Chiro n k (Several s k a)) ->
    (Digit r a) ->
    Chiro (l + n * s + r) k a

data Digit (n :: Nat) a where
  One :: a -> Digit 1 a
  Two :: a -> a -> Digit 2 a
  Three :: a -> a -> a -> Digit 3 a

digitToList :: Digit n a -> [a]
digitToList (One a) = [a]
digitToList (Two a1 a2) = [a1, a2]
digitToList (Three a1 a2 a3) = [a1, a2, a3]

data Several (n :: Nat) k a where
  STwo :: k -> a -> a -> Several 2 k a -- FIXME maybe wrap a Digit?
  SThree :: k -> a -> a -> a -> Several 3 k a

mkSeveral :: Measure k a => Digit n a -> Either a (Several n k a)
mkSeveral digit@(Two a1 a2) = Right $ STwo (measure digit) a1 a2
mkSeveral digit@(Three a1 a2 a3) = Right $ SThree (measure digit) a1 a2 a3

class Monoid k => Measure k a where
  measure :: a -> k

-- instance (Foldable t, Measure k a) => Measure k (t a) where
instance Measure k a => Measure k [a] where
  measure = foldMap measure

instance Measure k a => Measure k (Digit n a) where
  measure = measure . digitToList

instance Measure k a => Measure k (Several n k a) where
    measure (STwo k _ _) = k
    measure (SThree k _ _ _) = k

instance Measure k a => Measure k (Chiro n k a) where
  measure Empty = mempty
  measure (Single a) = measure a
  measure (Tree key _ _ _) = key

(<.>) :: Measure k a => Chiro nL k a -> Chiro nR k a -> Chiro (nL + nR) k a
Empty <.> chiro = chiro

chiro <.> Empty = chiro

Single valueL <.> Single valueR = Tree
    (measure valueL <> measure valueR)
    (One valueL)
    Empty
    (One valueR)

Single value <.> Tree key (One a) deep right = Tree
    (measure value <> key)
    (Two value a)
    deep
    right

Single value <.> Tree key (Two a1 a2) deep right = Tree
    (measure value <> key)
    (Three value a1 a2)
    deep
    right

Single value <.> Tree key (Three a1 a2 a3) deep right = Tree
    (measure value <> key)
    (One value)
    (mkSThree a1 a2 a3 <| deep)
    right

  {- TODO
  Tree { right = One a, .. } <> Single value = Tree
    { key = key <> key
    , right = Two a value
    , ..
    }

  Tree { right = Two a1 a2, .. } <> Single value = Tree
    { key = key <> key
    , right = Three a1 a2 value
    , ..
    }

  Tree { right = Three a1 a2 a3, .. } <> Single value = Tree
    { key = key <> key
    , right = One value
    , deep = deep |> mkSThree a1 a2 a3
    , ..
    }
  -}

-- Not sure whether this is actually as performant as I need it
Tree keyL left deepL mergeR <.> Tree keyR mergeL deepR right = Tree
  (keyL <> keyR)
  left
  (deepL tleft <.> mergeDigits mergeR mergeL <.> deepR tright)
  right

-- data ZeroOneTwo a = Zero | ZOne a | ZTwo a a

-- glue :: Chiro n k a -> ZeroOneTwo -> Chiro n k a -> Chiro n k a
-- glue

-- data TwoDigits a = DTwo a a | DThree a a a | DFour a a a a | DFive a a a a a | DSix a a a a a a

-- instance Measure k a => Measure k (TwoDigits a) where

-- glue :: Measure k a => Chiro k (Several n k a) -> TwoDigits a -> Chiro k (Several n k a) -> Chiro k (Several n k a)
-- glue tleft td@(DTwo a1 a2) tright = tleft <> Single (mkSTwo a1 a2) <> tright
-- glue tleft td@(DThree a1 a2 a3) tright = tleft <> Single (mkSThree a1 a2 a3) <> tright
-- glue tleft td@(DFour a1 a2 a3 a4) tright = tleft <> Single (mkSTwo a1 a2) <> Single (mkSTwo a1 a2) <> tright

mkEmpty :: Measure k a => Several n k a -> Several n k a -> Chiro n k (Several n k a)
mkEmpty sLeft sRight = Tree
  (measure sLeft <> measure sRight)
  Empty
  (One sLeft)
  (One sRight)

mergeDigits :: Measure k a => Digit n a -> Digit n a -> Chiro n k (Several n k a)
mergeDigits (One a1) (One a2) = Single $ mkSTwo a1 a2
mergeDigits (One a1) (Two a2 a3) = Single $ mkSThree a1 a2 a3
mergeDigits (One a1) (Three a2 a3 a4) = mkEmpty (mkSTwo a1 a2) (mkSTwo a3 a4)
mergeDigits (Two a1 a2) (Three a3 a4 a5) = mkEmpty (mkSTwo a1 a2) (mkSThree a3 a4 a5)
mergeDigits (Three a1 a2 a3) (Three a4 a5 a6) = mkEmpty (mkSThree a1 a2 a3) (mkSThree a4 a5 a6)

(<|) :: Measure k a => a -> Chiro n k a -> Chiro n k a
a <| chiro = Single a <> chiro

(|>) :: Measure k a => Chiro n k a -> a -> Chiro n k a
(|>) = flip (<|)

mkSTwo :: Measure k a => a -> a -> Several n k a
mkSTwo a1 a2 =
  let queueKey = fold $ measure <$> [a1, a2]
  in STwo queueKey a1 a2

mkSThree :: Measure k a => a -> a -> a -> Several n k a
mkSThree a1 a2 a3 =
  let queueKey = fold $ measure <$> [a1, a2, a3]
  in SThree queueKey a1 a2 a3
