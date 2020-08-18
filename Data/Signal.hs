module Data.Signal(
    -- data types
    Rate, Amplitude, Frecuency, Phase, Time, Signal(..),
    -- make waves
    sineWave, squareWave, triangleWave, sawWave,
    -- combinators
    mapSamples, mapSignal, time, decay,
    -- common sample rates
    ctrRate, audRate,
    -- notes
    zero,
    c, d, e, f, g, a, b,
    csharp, dsharp, fsharp, gsharp, asharp,
    dflat, eflat, gflat, aflat, bflat,
    -- format
    toWave
) where


import Data.Semigroup(Semigroup(..))
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

-- | Phase (in radians)
-- The phase of a periodic function of some real variable (such as time) is an
-- angle representing the number of periods spanned by that variable.
type Phase = Double

-- | Time
type Time = Double

-- | Signal
data Signal = Signal { runSignal :: Rate -> [Double] }

instance Semigroup Signal where
    (Signal f) <> (Signal g) = Signal (\rate -> f rate ++ g rate)

instance Monoid Signal where
    mempty = Signal (\_rate -> [])
    mappend = (<>)

instance Num Signal where
    (Signal f) + (Signal g) = Signal (\rate -> zipWaves (+) (f rate) (g rate))
    (Signal f) - (Signal g) = Signal (\rate -> zipWaves (-) (f rate) (g rate))
    (Signal f) * (Signal g) = Signal (\rate -> zipWaves (*) (f rate) (g rate))
    abs (Signal f) = Signal ((map abs) . f)
    signum (Signal f) = Signal ((map signum) . f)
    fromInteger n = Signal (\rate -> repeat (fromIntegral n))


-- | MAKE SIGNALS

-- | sineWave
-- A sine wave or sinusoid is a mathematical curve that describes a smooth
-- periodic oscillation. A sine wave is a continuous wave.
-- Its form as a function of time t is: 
--
-- y(t) = A * sin(2*pi*f*t + phi)
--
-- where A is the amplitude, f is the frecuency and phi is the phase.
sineWave :: Amplitude -> Phase -> Frecuency -> Signal
sineWave a phi f = Signal (\rate ->
    let rate' = fromIntegral rate
    in map (\t -> a * sin (2*pi*f*(t/rate') + phi)) [0..])

-- | squareWave
-- A square wave is a non-sinusoidal periodic waveform in which the amplitude
-- alternates at a steady frequency between fixed minimum and maximum values,
-- with the same duration at minimum and maximum.
-- Its form as a function of time t is: 
--
-- y(t) = A * sgn(sin(2*pi*f*t + phi))
--
-- where A is the amplitude, f is the frecuency and phi is the phase.
squareWave :: Amplitude -> Phase -> Frecuency -> Signal
squareWave a phi f = Signal (\rate ->
    let rate' = fromIntegral rate
    in map (\t -> a * signum (sin (2*pi*f*(t/rate') + phi))) [0..])

-- | triangleWave
-- A triangle wave is a non-sinusoidal waveform named for its triangular shape.
-- It is a periodic, piecewise linear, continuous real function. 
-- Its form as a function of time t is: 
--
-- y(t) = 2*A / pi * asin(sin(2*pi*f*t + phi))
--
-- where A is the amplitude, f is the frecuency and phi is the phase.
triangleWave :: Amplitude -> Phase -> Frecuency -> Signal
triangleWave a phi f = Signal (\rate ->
    let rate' = fromIntegral rate
    in map (\t -> (2*a / pi) * asin (sin (2*pi*f*(t/rate') + phi))) [0..])

-- | sawWave
-- The sawtooth wave is a kind of non-sinusoidal waveform. The convention is
-- that a sawtooth wave ramps upward and then sharply drops. In a reverse
-- (or inverse) sawtooth wave, the wave ramps downward and then sharply rises.
-- Its form as a function of time t is: 
--
-- y(t) = -2*A * ((t+phi)*f - floor(0.5+(t+phi)*f))
--
-- where A is the amplitude, f is the frecuency and phi is the phase.
sawWave :: Amplitude -> Phase -> Frecuency -> Signal
sawWave a phi f = Signal (\rate ->
    let rate' = fromIntegral rate
    in map (\t -> 2 * a * (((t+phi)/rate')*f - fromIntegral(floor (0.5 + ((t+phi)/rate')*f)))) [0..])


-- | SIGNAL COMBINATORS

-- | mapSamples
-- Map the samples of the signal.
mapSamples :: ([Double] -> [Double]) -> Signal -> Signal
mapSamples g (Signal f) = Signal (g . f)

-- | mapSignal
-- Map the function of the signal.
mapSignal :: (Rate -> [Double] -> [Double]) -> Signal -> Signal
mapSignal g (Signal f) = Signal (\rate -> g rate (f rate))

-- | time
-- Take the first n seconds of a signal.
time :: Time -> Signal -> Signal
time t = mapSignal (\rate -> takeZero (round (t * fromIntegral rate)))

-- | decay
-- Decay the signal the last n seconds.
decay :: Time -> Signal -> Signal
decay t (Signal f) = Signal (\rate ->
    let xs = f rate
        m = round (t * fromIntegral rate)
        n = length xs - m
    in take n xs ++ zipWith (*) [1, 1-1/(fromIntegral m)..] (drop n xs))


-- | COMMON RATES

-- | ctrRate
-- 4410 Hz.
ctrRate :: Rate
ctrRate = 4410

-- | audRate
-- 44100 Hz.
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
g = (&) 392.00
a = (&) 440.00
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


-- | FORMAT AND EXPORT FUNCTIONS

-- | toWave
-- Generate a wave from a signal, that can be stored as a .wav file by the WAVE
-- package:
--
-- $ cabal install WAVE
--
toWave :: Rate -> Signal -> WAVE
toWave rate (Signal f) = let samples = f rate
                             header = WAVEHeader 1 rate 32 (Just $ length samples)
                             body = map (pure . doubleToSample) samples
                         in WAVE header body


-- | AUXILIAR OPERATIONS (DO NOT EXPORT)

-- | zipWaves
zipWaves :: (Double -> Double -> Double) -> [Double] -> [Double] -> [Double]
zipWaves f [] [] = []
zipWaves f [] (y:ys) = f 0 y : zipWaves f [] ys
zipWaves f (x:xs) [] = f x 0 : zipWaves f xs []
zipWaves f (x:xs) (y:ys) = f x y : zipWaves f xs ys

-- | takeZero
takeZero :: Int -> [Double] -> [Double]
takeZero n xs = take (takeZero' n (drop n xs)) xs
    where takeZero' n [] = n
          takeZero' n [_] = n
          takeZero' n (a:b:xs) = if a == 0 || signum a /= signum b
                                 then n+1
                                 else takeZero' (n+1) (b:xs)