{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
module Data.Chiro where
import System.Posix.Internals (c_ftruncate)
import Data.Foldable (fold)

data Chiro k a
  = Empty
  | Single a
  | TreeH -- FIXME Refactor as separate type TreeH
    { key :: k
    , left :: Digit a
    , deep :: Chiro k (Several k a)
    , right :: Digit a
    }
  | TreeV
    { key :: k
    , up :: Digit a
    , deep :: Chiro k (Several k a)
    , down :: Digit a
    }

transpose :: Chiro k a -> Chiro k a
transpose Empty = Empty
transpose (Single a) = Single a
transpose TreeH { .. } = TreeV { deep = transpose deep, up = left, down = right, .. }
transpose TreeV { .. } = TreeH { deep = transpose deep, left = up, right = down, .. }

data Digit a
  = One a
  | Two a a
  | Three a a a

digitToList :: Digit a -> [a]
digitToList (One a) = [a]
digitToList (Two a1 a2) = [a1, a2]
digitToList (Three a1 a2 a3) = [a1, a2, a3]

data Several k a
  = STwo k a a
  | SThree k a a a

mkSeveral :: Measure k a => Digit a -> Either a (Several k a)
mkSeveral digit@(Two a1 a2) = Right $ STwo (measure digit) a1 a2
mkSeveral digit@(Three a1 a2 a3) = Right $ SThree (measure digit) a1 a2 a3

class Monoid k => Measure k a where
  measure :: a -> k

-- instance (Foldable t, Measure k a) => Measure k (t a) where
instance Measure k a => Measure k [a] where
  measure = foldMap measure

instance Measure k a => Measure k (Digit a) where
  measure = measure . digitToList

instance Measure k a => Measure k (Several k a) where
    measure (STwo k _ _) = k
    measure (SThree k _ _ _) = k

instance Measure k a => Measure k (Chiro k a) where
  measure Empty = mempty
  measure (Single a) = measure a
  measure TreeH { .. } = key

instance Measure k a => Semigroup (Chiro k a) where
  Empty <> chiro = chiro

  chiro <> Empty = chiro

  Single valueL <> Single valueR = TreeH
    { key = measure valueL <> measure valueR
    , left = One valueL
    , deep = Empty
    , right = One valueR
    }

  Single value <> tree@TreeH { left = One a, .. } = TreeH
    { key = key <> key
    , left = Two value a
    , ..
    }

  Single value <> tree@TreeH { left = Two a1 a2, .. } = TreeH
    { key = key <> key
    , left = Three value a1 a2
    , ..
    }

  Single value <> tree@TreeH { left = Three a1 a2 a3, .. } = TreeH
    { key = key <> key
    , left = One value
    , deep = mkSThree a1 a2 a3 <| deep
    , ..
    }

  TreeH { right = One a, .. } <> Single value = TreeH
    { key = key <> key
    , right = Two a value
    , ..
    }

  TreeH { right = Two a1 a2, .. } <> Single value = TreeH
    { key = key <> key
    , right = Three a1 a2 value
    , ..
    }

  TreeH { right = Three a1 a2 a3, .. } <> Single value = TreeH
    { key = key <> key
    , right = One value
    , deep = deep |> mkSThree a1 a2 a3
    , ..
    }

-- Not sure whether this is actually as performant as I need it
  tleft@TreeH { right = mergeR, left } <> tright@TreeH { left = mergeL, right } = TreeH
    { key = key tleft <> key tright
    , deep = deep tleft <> mergeDigits mergeR mergeL <> deep tright
    , ..
    }

-- data ZeroOneTwo a = Zero | ZOne a | ZTwo a a

-- glue :: Chiro k a -> ZeroOneTwo -> Chiro k a -> Chiro k a
-- glue

-- data TwoDigits a = DTwo a a | DThree a a a | DFour a a a a | DFive a a a a a | DSix a a a a a a

-- instance Measure k a => Measure k (TwoDigits a) where

-- glue :: Measure k a => Chiro k (Several k a) -> TwoDigits a -> Chiro k (Several k a) -> Chiro k (Several k a)
-- glue tleft td@(DTwo a1 a2) tright = tleft <> Single (mkSTwo a1 a2) <> tright
-- glue tleft td@(DThree a1 a2 a3) tright = tleft <> Single (mkSThree a1 a2 a3) <> tright
-- glue tleft td@(DFour a1 a2 a3 a4) tright = tleft <> Single (mkSTwo a1 a2) <> Single (mkSTwo a1 a2) <> tright

mkEmpty :: Measure k a => Several k a -> Several k a -> Chiro k (Several k a)
mkEmpty sLeft sRight = TreeH
  { key = measure sLeft <> measure sRight
  , deep = Empty
  , left = One sLeft
  , right = One sRight
  }

mergeDigits :: Measure k a => Digit a -> Digit a -> Chiro k (Several k a)
mergeDigits (One a1) (One a2) = Single $ mkSTwo a1 a2
mergeDigits (One a1) (Two a2 a3) = Single $ mkSThree a1 a2 a3
mergeDigits (One a1) (Three a2 a3 a4) = mkEmpty (mkSTwo a1 a2) (mkSTwo a3 a4)
mergeDigits (Two a1 a2) (Three a3 a4 a5) = mkEmpty (mkSTwo a1 a2) (mkSThree a3 a4 a5)
mergeDigits (Three a1 a2 a3) (Three a4 a5 a6) = mkEmpty (mkSThree a1 a2 a3) (mkSThree a4 a5 a6)

(<|) :: Measure k a => a -> Chiro k a -> Chiro k a
a <| chiro = Single a <> chiro

(|>) :: Measure k a => Chiro k a -> a -> Chiro k a
(|>) = flip (<|)

mkSTwo :: Measure k a => a -> a -> Several k a
mkSTwo a1 a2 =
  let queueKey = fold $ measure <$> [a1, a2]
  in STwo queueKey a1 a2

mkSThree :: Measure k a => a -> a -> a -> Several k a
mkSThree a1 a2 a3 =
  let queueKey = fold $ measure <$> [a1, a2, a3]
  in SThree queueKey a1 a2 a3

instance Measure k a => Monoid (Chiro k a) where
  mempty = Empty
