module Nut.Color.Spectrum.Hunter where

-- Hunter L,a,b coordinates for a color given a specified standard observer model and standard illuminant
data HunterLab o i a = HunterLab a a a

-- metamerism :: Floating a => HunterLab o i a -> HunterLab o j a -> a
