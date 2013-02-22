module Nut.Color.Spectrum.XYZ
  ( XYZ(..)
  ) where

import Control.Applicative
import Data.Foldable
import Data.Traversable
import Data.Monoid
import Nut.Numeric.Double
import Nut.Numeric.Float

data XYZ a = XYZ a a a deriving (Eq,Ord,Show,Read)
instance Functor XYZ where
  fmap f (XYZ x y z) = XYZ (f x) (f y) (f z)

instance Applicative XYZ where
  pure a = XYZ a a a
  XYZ fx fy fz <*> XYZ x y z = XYZ (fx x) (fy y) (fz z)

instance Traversable XYZ where
  traverse f (XYZ x y z) = XYZ <$> f x <*> f y <*> f z

instance Foldable XYZ where
  foldMap f (XYZ x y z) = f x `mappend` f y `mappend` f z

instance Num a => Num (XYZ a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  (-) = liftA2 (-)
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger

instance Fractional a => Fractional (XYZ a) where
  (/) = liftA2 (/)
  recip = fmap recip
  fromRational = pure . fromRational

instance Floating a => Floating (XYZ a) where
  pi = pure pi
  exp = fmap exp
  sqrt = fmap sqrt
  log = fmap log
  (**) = liftA2 (**)
  logBase = liftA2 logBase
  sin = fmap sin
  cos = fmap cos
  tan = fmap tan
  asin = fmap asin
  acos = fmap acos
  atan = fmap atan
  sinh = fmap sinh
  cosh = fmap cosh
  tanh = fmap tanh
  asinh = fmap asinh
  acosh = fmap acosh
  atanh = fmap atanh

instance FromFloat a => FromFloat (XYZ a) where
  fromFloat = pure . fromFloat

instance FromDouble a => FromDouble (XYZ a) where
  fromDouble = pure . fromDouble
