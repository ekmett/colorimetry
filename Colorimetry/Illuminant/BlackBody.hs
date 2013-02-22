module Colorimetry.Illuminant.BlackBody
  ( blackBody
  , idealizedD65
  ) where

-- | A perfect black body emitter. Gives the energy for a given temperature blackbody emitter
-- in power per m^2 of area per nm of wavelength using Planck's energy distribution formula.
-- given the temperature of the emitter in degrees Kelven and the wavelength 
blackBody :: Floating a => a -> a -> a
blackBody temp wlnm= (3.74183e-16 * (wlm ^^ (-5 :: Int))) * 44 / (exp(1.4388e-2 / (wlm * temp)) - 1)
  where wlm = wlnm * 1e-9
{-# INLINE blackBody #-}

-- | Idealized approximation of the cie standard illuminant D65
idealizedD65 :: Double -> Double
idealizedD65 = blackBody 6504
