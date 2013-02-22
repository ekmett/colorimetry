module Color.Spectrum.Observer where

class Num a => ObserverModel m a | m -> a where
  chromaticResponseWindow :: m -> (a, a)
  chromaticResponse :: m -> a -> a -> XYZ a
