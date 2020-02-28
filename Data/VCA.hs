{-# LANGUAGE MultiParamTypeClasses #-}

module Data.VCA(
    VCA(..)
) where

import Data.Oscillator(Oscillator(..), OscillatorModule(..))

data VCA = VCA {
    getVCALevel :: Float,
    getVCAControlVoltage :: Maybe Oscillator
}

instance OscillatorModule VCA where
    us >>> (VCA level Nothing) = map (\u -> Oscillator $ \t -> getWave u t * level) us
    us >>> (VCA level (Just cv)) = map (\u -> Oscillator $ \t -> getWave u t * (getWave cv) t * level) us