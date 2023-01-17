module PSL.Eval.Interval (
  LowerBound,
  olb,
  clb,
  UpperBound,
  oub,
  cub,
  Interval (..),
  never,
  always,
  union,
  intersection,
  contains,
  singleton,
  from,
  to,
  member,
  before,
  after,
) where

import Prettyprinter (Pretty (pretty))

data Extended a
  = ExNegInf
  | ExFin a
  | ExPosInf
  deriving stock (Eq, Ord, Functor)

instance Bounded (Extended a) where
  minBound = ExNegInf
  maxBound = ExPosInf

instance Pretty a => Pretty (Extended a) where
  pretty ExNegInf = "-∞"
  pretty ExPosInf = "+∞"
  pretty (ExFin x) = pretty x

data Closure = ClosureOpen | ClosureClosed
  deriving stock (Eq, Ord)

-- | The lower bound of an interval.
data LowerBound a = LowerBound (Extended a) Closure
  deriving stock (Eq, Functor)

instance Ord a => Ord (LowerBound a) where
  LowerBound x xc <= LowerBound y yc
    | x == y = xc > yc
    | otherwise = x <= y

instance Bounded (LowerBound a) where
  minBound = LowerBound ExNegInf ClosureClosed
  maxBound = LowerBound ExPosInf ClosureOpen

instance Pretty a => Pretty (LowerBound a) where
  pretty (LowerBound ExNegInf _) = "(-∞"
  pretty (LowerBound ExPosInf _) = "(+∞"
  pretty (LowerBound (ExFin x) ClosureOpen) = "(" <> pretty x
  pretty (LowerBound (ExFin x) ClosureClosed) = "[" <> pretty x

-- | Make an open lower bound.
olb :: a -> LowerBound a
olb x = LowerBound (ExFin x) ClosureOpen

-- | Make a closed lower bound.
clb :: p -> LowerBound p
clb x = LowerBound (ExFin x) ClosureClosed

-- | The upper bound of an interval.
data UpperBound a = UpperBound (Extended a) Closure
  deriving stock (Eq, Functor)

instance Ord a => Ord (UpperBound a) where
  UpperBound x xc <= UpperBound y yc
    | x == y = xc <= yc
    | otherwise = x <= y

instance Bounded (UpperBound a) where
  minBound = UpperBound ExNegInf ClosureOpen
  maxBound = UpperBound ExPosInf ClosureClosed

instance Pretty a => Pretty (UpperBound a) where
  pretty (UpperBound ExNegInf _) = "-∞)"
  pretty (UpperBound ExPosInf _) = "+∞)"
  pretty (UpperBound (ExFin x) ClosureOpen) = pretty x <> ")"
  pretty (UpperBound (ExFin x) ClosureClosed) = pretty x <> "]"

-- | Make an open upper bound.
oub :: a -> UpperBound a
oub x = UpperBound (ExFin x) ClosureOpen

-- | Make a closed upper bound.
cub :: p -> UpperBound p
cub x = UpperBound (ExFin x) ClosureClosed

-- | A mathematical interval.
data Interval a = Interval
  { ivStart :: LowerBound a
  , ivEnd :: UpperBound a
  }
  deriving stock (Eq, Ord, Functor)

instance Pretty a => Pretty (Interval a) where
  pretty (Interval l r) = pretty l <> ", " <> pretty r

-- | An interval that contains nothing.
never :: Interval a
never = Interval maxBound minBound

-- | An interval that contains everything.
always :: Interval a
always = Interval minBound maxBound

-- | Union of two intervals. Takes the smaller lower bound and the larger upper bound.
union :: Ord a => Interval a -> Interval a -> Interval a
Interval l r `union` Interval l' r' = Interval (min l l') (max r r')

-- | Intersection of two intervals. Takes the larger lower bound and the smaller upper bound.
intersection :: Ord a => Interval a -> Interval a -> Interval a
Interval l r `intersection` Interval l' r' = Interval (max l l') (min r r')

-- | Checks if the former interval contains the latter interval fully.
contains :: Ord a => Interval a -> Interval a -> Bool
Interval l r `contains` Interval l' r' = l <= l' && r' <= r

-- | An interval containing a single value.
singleton :: a -> Interval a
singleton x = Interval (clb x) (cub x)

-- | An interval from a point to positive infinity.
from :: a -> Interval a
from x = Interval (clb x) maxBound

-- | An interval from negative infinity to a certain point.
to :: a -> Interval a
to x = Interval minBound (cub x)

-- | Checks if a value is within the interval.
member :: Ord a => a -> Interval a -> Bool
x `member` iv = iv `contains` singleton x

-- | Checks if a value is lower than the lower bound of the interval.
before :: Ord a => a -> Interval a -> Bool
x `before` (Interval lb _) = clb x < lb

-- | Checks if a value is higher than the upper bound of the interval.
after :: Ord a => a -> Interval a -> Bool
x `after` (Interval _ ub) = ub < cub x
