{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Mixer(
    Mixer(..)
) where

import Data.Oscillator(Oscillator(..), OscillatorModule(..))
import Data.VCA(VCA(..))

data Mixer = Mixer {
    getMixerLevels :: [Float],
    getMixerControlVoltages :: [Maybe Oscillator]
}

instance OscillatorModule Mixer where
    u >>> (Mixer levels cvs) = [sum $ map head $ zipWith (>>>) (map (:[]) u) (zipWith VCA levels cvs)]