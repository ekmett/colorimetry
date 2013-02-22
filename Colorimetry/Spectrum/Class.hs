{-# LANGUAGE ViewPatterns, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

module Nut.Color.Spectrum.Class
  ( Spectrum(..)
  , whiteness
  , yellowness
  ) where

import Nut.Color.Spectrum.RGB
import Nut.Color.Spectrum.XYZ

class (Floating s, Floating a) => Spectrum s a | s -> a where
  toXYZ :: s -> XYZ a
  toRGB :: s -> RGB a
  luminance :: s -> a
  luminance (toXYZ -> XYZ _ y _) = y

instance Floating a => Spectrum (XYZ a) a where
  toXYZ = id
  toRGB (XYZ x y z) = RGB (3.240479*x - 1.537150*y - 0.498535*z)
                          (-0.969256*x + 1.875991*y + 0.041556*z)
                          (0.055648*x - 0.204043*y + 1.057311*z)
  luminance (XYZ _ y _) = y

instance Floating a => Spectrum (RGB a) a where
  toRGB = id
  toXYZ (RGB r g b) = XYZ
    (0.412453*r + 0.357580*g + 0.180423*b)
    (0.212671*r + 0.715160*g + 0.072169*b)
    (0.019334*r + 0.119193*g + 0.950227*b)
  luminance (RGB r g b) = 0.212671*r + 0.715160*g + 0.072169*b

-- calculate whiteness index by ASTM method E313
whiteness :: Spectrum s a => s -> a
whiteness (toXYZ -> XYZ _ y z) = 3.388*z - 3*y

-- calculate yellowness index by ASTM method D1925 (withdrawn in 1995)
yellowness :: Spectrum s a => s -> a
yellowness (toXYZ -> XYZ x y z) = (128*x-106*z)/y
