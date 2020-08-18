module Data.Signal(
    Rate, Amplitude, Frecuency, Phase, Time, Signal,
    sinusoid,
    smap, sjoin, stime,
    ctrRate, audRate,
    zero,
    c, d, e, f, g, a, b,
    csharp, dsharp, fsharp, gsharp, asharp,
    dflat, eflat, gflat, aflat, bflat,
    toWave
) where


import Data.Function((&))
import Data.WAVE(doubleToSample, WAVE(..), WAVEHeader(..))


-- | DATA TYPES

-- | Rate (in Hz)
-- A commonly seen unit of sampling. Samples per second.
type Rate = Int

-- | Amplitude
-- The amplitude of a periodic variable is a measure of its change in a single
-- period.
type Amplitude = Double

-- | Frequency (in Hz)
-- The frequency is the number of occurrences of a repeating event per unit
-- of time. Sound propagates as mechanical vibration waves of pressure and
-- displacement. In general, frequency components of a sound determine its
-- timbre.
type Frecuency = Double

-- | Phase (in degrees)
-- The phase of a periodic function of some real variable (such as time) is an
-- angle representing the number of periods spanned by that variable.
type Phase = Double

-- | Time
type Time = Double

-- | Signal
data Signal = Signal {
    getRate :: Rate,
    getSamples ::  [Double]
} deriving (Show, Eq)


-- | MAKE SIGNALS

-- | sinusoid
sinusoid :: Rate -> Amplitude -> Phase -> Frecuency -> Signal
sinusoid rate a phi f = let w = 2*pi*f
                            rate' = fromIntegral rate
                        in Signal rate $ cycle (map (\t -> a * sin (w*(t/rate') + phi)) [0..fromIntegral rate])


-- | SIGNAL COMBINATORS

-- | smap
-- Map the samples of the signal.
smap :: (Double -> Double) -> Signal -> Signal
smap f (Signal rate samples) = Signal rate (map f samples)

-- | sjoin
-- Join a list of signals into an only one signal.
sjoin :: Rate -> [Signal] -> Signal
sjoin rate xs = Signal rate (concat $ map getSamples xs)

-- | stime
-- Take the first n seconds of a signal.
stime :: Time -> Signal -> Signal
stime t (Signal rate samples) = Signal rate $ take (round (t*fromIntegral rate)) samples


-- | COMMON RATES

-- | ctrRate (4410 Hz)
ctrRate :: Rate
ctrRate = 4410

-- | audRate (44100 Hz)
-- In digital audio, 44100 Hz is a common sampling frequency. Analog audio is
-- recorded by sampling it 44100 times per second, and then these samples are
-- used to reconstruct the audio signal when playing it back. 
audRate :: Rate
audRate = 44100


-- | MUSICAL NOTES
-- A note is a symbol denoting a musical sound.
-- Notes can represent the pitch and duration of a sound in musical notation.

-- | Empty sound.
zero :: (Frecuency -> Signal) -> Signal
zero = (&) 0

-- | 12 notes of a chromatic scale built on C.
c, d, e, f, g, a, b :: (Frecuency -> Signal) -> Signal
c = (&) 261.63
d = (&) 293.66
e = (&) 329.63
f = (&) 349.23
g = (&) 392.0
a = (&) 440.0
b = (&) 493.88

csharp, dsharp, fsharp, gsharp, asharp :: (Frecuency -> Signal) -> Signal
csharp = (&) 277.18
dsharp = (&) 311.13
fsharp = (&) 369.99
gsharp = (&) 415.30
asharp = (&) 466.16

dflat, eflat, gflat, aflat, bflat :: (Frecuency -> Signal) -> Signal
dflat = (&) 277.18
eflat = (&) 311.13
gflat = (&) 369.99
aflat = (&) 415.30
bflat = (&) 466.16


-- | DATA REPRESENTATION AND EXPORT FUNCTIONS

-- | toWave
-- Generate a wave from a signal, that can be stored as a .wav file by the WAVE
-- package:
--
-- $ cabal install WAVE
--
toWave :: Signal -> WAVE
toWave (Signal rate samples) = let header = WAVEHeader 1 rate 32 (Just $ length samples)
                                   body = map ((:[]).doubleToSample) samples
                               in WAVE header body
