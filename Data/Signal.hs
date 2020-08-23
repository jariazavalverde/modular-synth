module Data.Signal(
    -- data types
    Signal(..), SampleRate,
    -- combinators
    mapSignal, zipSignal,
    -- common sample rates
    audioRate,
    -- format
    signalToWave
) where


import Data.WAVE(doubleToSample, WAVE(..), WAVEHeader(..)) -- WAVE package


-- | DATA TYPES

-- | Sample rate (in Hz)
-- A commonly seen unit of sampling. Samples per second.
type SampleRate = Int

-- | Signal
newtype Signal a = Signal {
    runSignal :: a -> a
}

instance Num a => Num (Signal a) where
    (+) = zipSignal (+)
    (-) = zipSignal (-)
    (*) = zipSignal (*)
    abs = mapSignal abs
    signum = mapSignal signum
    fromInteger = Signal . const . fromInteger

instance Fractional a => Fractional (Signal a) where
    (/) = zipSignal (/)
    fromRational = Signal . const . fromRational

instance Floating a => Floating (Signal a) where
    pi = Signal (const pi)
    exp = mapSignal exp
    log = mapSignal log
    sin = mapSignal sin
    cos = mapSignal cos
    asin = mapSignal asin
    acos = mapSignal acos
    atan = mapSignal atan
    sinh = mapSignal sinh
    cosh = mapSignal cosh
    asinh = mapSignal asinh
    acosh = mapSignal acosh
    atanh = mapSignal atanh


-- | SIGNAL COMBINATORS

-- | Map signal
-- Map the samples of a signal.
mapSignal :: (a -> a) -> Signal a -> Signal a
mapSignal f (Signal g) = Signal (f . g)

-- | Zip signals
-- Zip the samples of two signals with the function given as the first argument.
zipSignal :: (a -> a -> a) -> Signal a -> Signal a -> Signal a
zipSignal op (Signal f) (Signal g) = Signal (\t -> f t `op` g t)


-- | COMMON RATES

-- | audRate
-- 44100 Hz.
-- In digital audio, 44100 Hz is a common sampling frequency. Analog audio is
-- recorded by sampling it 44100 times per second, and then these samples are
-- used to reconstruct the audio signal when playing it back. 
audioRate :: SampleRate
audioRate = 44100


-- | FORMAT AND EXPORT FUNCTIONS

-- | toWave
-- Generate a wave from a signal, that can be stored as a .wav file by the WAVE
-- package:
--
-- $ cabal install WAVE
--
signalToWave ::
    (Num a, RealFrac a, Fractional a, Real a, Enum a) =>
    SampleRate -> a -> Signal a -> WAVE
signalToWave r t (Signal f) = let r' = fromIntegral r
                                  len = floor (r' * t)
                                  xs = take len (map (f . (/ r')) [0..])
                                  header = WAVEHeader 1 r 32 (Just len)
                                  body = map (pure . doubleToSample . realToFrac) xs
                              in WAVE header body