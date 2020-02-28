{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Oscillator(
    Oscillator(..),
    OscillatorModule(..),
    sinusoid
) where

type Period = Float
type Frecuency = Float
type Amplitude = Float
type Phase = Float

data Oscillator = Oscillator {
    getWave :: Float -> Float
}

instance Num Oscillator where
    u + v = Oscillator $ \t -> getWave u t + getWave v t
    u - v = Oscillator $ \t -> getWave u t - getWave v t
    u * v = Oscillator $ \t -> getWave u t * getWave v t
    negate u = Oscillator $ \t -> -(getWave u t)
    abs u = Oscillator $ \t -> abs (getWave u t)
    signum u = Oscillator $ \t -> signum (getWave u t)
    fromInteger x = Oscillator $ \_ -> fromIntegral x

class OscillatorModule m where
    (>>>) :: [Oscillator] -> m -> [Oscillator]

sinusoid :: Amplitude -> Phase -> Frecuency -> Oscillator
sinusoid a phi f = Oscillator $ \t -> a * sin (2*pi*f*t + phi)