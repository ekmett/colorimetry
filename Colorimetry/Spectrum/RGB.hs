module Nut.Color.Spectrum.RGB
  ( RGB(..)
  ) where

import Control.Applicative
import Data.Foldable
import Data.Traversable
import Data.Monoid
import Nut.Numeric.Double
import Nut.Numeric.Float

data RGB a = RGB a a a

instance Functor RGB where
  fmap f (RGB r g b) = RGB (f r) (f g) (f b)

instance Applicative RGB where
  pure a = RGB a a a

instance Foldable RGB where
  foldMap f (RGB r g b) = f r `mappend` f g `mappend` f b

instance Traversable RGB where
  traverse f (RGB r g b) = RGB <$> f r <*> f g <*> f b

instance Num a => Num (RGB a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  (-) = liftA2 (-)
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger

instance Fractional a => Fractional (RGB a) where
  (/) = liftA2 (/)
  recip = fmap recip
  fromRational = pure . fromRational

instance Floating a => Floating (RGB a) where
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

instance FromFloat a => FromFloat (RGB a) where
  fromFloat = pure . fromFloat

instance FromDouble a => FromDouble (RGB a) where
  fromDouble = pure . fromDouble

