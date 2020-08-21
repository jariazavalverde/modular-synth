module Data.Signal(
    -- data types
    Rate, Amplitude, Frecuency, Phase, Time, Signal(..),
    Attack, Decay, Sustain, Retain,
    -- make waves
    sineWave, squareWave, triangleWave, sawWave,
    -- combinators
    time, time', adsr, (+>), join, start,
    -- common sample rates
    ctrRate, audRate,
    -- format
    toWave, dropSamples
) where


import Control.Applicative(liftA2)
import Data.Semigroup(Semigroup(..))
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
type Phase a = (a, Ordering)

-- | Time (in seconds)
-- The time is the measured or measurable period during which an action, process, or
-- condition exists or continues.
type Time = Double

-- | Attack
-- Attack is the time taken for initial run-up of level from nil to peak.
type Attack = Double

-- | Decay
-- Decay is the time taken for the subsequent run down from the attack level to
-- the designated sustain level.
type Decay = Double

-- | Sustain
-- Sustain is the level during the main sequence of the sound's duration.
type Sustain = Double

-- | Release
-- Release is the time taken for the level to decay from the sustain level to
-- zero.
type Retain = Double

-- | Signal
data Signal a = Signal { runSignal :: Rate -> [a] }

instance Functor Signal where
    fmap g (Signal f) = Signal ((map g) . f)

instance Applicative Signal where
    pure x = Signal (\_rate -> repeat x)
    (Signal f) <*> (Signal g) = Signal (\rate -> zipWith ($) (f rate) (g rate))

instance Semigroup (Signal a) where
    (Signal f) <> (Signal g) = Signal (\rate -> f rate ++ g rate)

instance Monoid (Signal a) where
    mempty = Signal (\_rate -> [])
    mappend = (<>)

instance Num a => Num (Signal a) where
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)
    abs = fmap abs
    signum = fmap signum
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
sineWave :: Frecuency -> Phase Double -> Signal Double
sineWave f phi = Signal (\rate ->
    let rate' = fromIntegral rate
        xs = map (\t -> sin (2*pi*f*(t/rate'))) [0..]
    in if fst phi == 0 then xs else dropSamples phi xs)


-- | squareWave
-- A square wave is a non-sinusoidal periodic waveform in which the amplitude
-- alternates at a steady frequency between fixed minimum and maximum values,
-- with the same duration at minimum and maximum.
-- Its form as a function of time t is: 
--
-- y(t) = A * sgn(sin(2*pi*f*t + phi))
--
-- where A is the amplitude, f is the frecuency and phi is the phase.
squareWave :: Frecuency -> Phase Double -> Signal Double
squareWave f phi = Signal (\rate ->
    let rate' = fromIntegral rate
        xs = map (\t -> signum (sin (2*pi*f*(t/rate')))) [0..]
    in if fst phi == 0 then xs else dropSamples phi xs)

-- | triangleWave
-- A triangle wave is a non-sinusoidal waveform named for its triangular shape.
-- It is a periodic, piecewise linear, continuous real function. 
-- Its form as a function of time t is: 
--
-- y(t) = 2*A / pi * asin(sin(2*pi*f*t + phi))
--
-- where A is the amplitude, f is the frecuency and phi is the phase.
triangleWave :: Frecuency -> Phase Double -> Signal Double
triangleWave f phi = Signal (\rate ->
    let rate' = fromIntegral rate
        xs = map (\t -> (2/pi) * asin (sin (2*pi*f*(t/rate')))) [0..]
    in if fst phi == 0 then xs else dropSamples phi xs)

-- | sawWave
-- The sawtooth wave is a kind of non-sinusoidal waveform. The convention is
-- that a sawtooth wave ramps upward and then sharply drops. In a reverse
-- (or inverse) sawtooth wave, the wave ramps downward and then sharply rises.
-- Its form as a function of time t is: 
--
-- y(t) = -2*A * ((t+phi)*f - floor(0.5+(t+phi)*f))
--
-- where A is the amplitude, f is the frecuency and phi is the phase.
sawWave :: Frecuency -> Phase Double -> Signal Double
sawWave f phi = Signal (\rate ->
    let rate' = fromIntegral rate
        xs = map (\t -> 2 * ((t/rate')*f - fromIntegral(floor (0.5 + (t/rate')*f)))) [0..]
    in if fst phi == 0 then xs else dropSamples phi xs)


-- | SIGNAL COMBINATORS

-- | time
-- Take the first n seconds of a signal.
time :: (Eq a, Num a) => Time -> Signal a -> Signal a
time t (Signal f) = Signal (\rate -> take (round (t * fromIntegral rate)) (f rate))

-- | time'
-- Take the first n % of a signal.
time' :: (Eq a, Num a) => Time -> Signal a -> Signal a
time' t (Signal f) = Signal (\rate -> let xs = f rate
                                      in take (round (t * fromIntegral (length xs))) xs)

-- | adsr
-- An envelope describes how a sound changes over time. The most common kind of
-- envelope generator has four stages: attack, decay, sustain, and release.
adsr :: Attack -> Decay -> Sustain -> Retain -> Signal Double -> Signal Double
adsr a d s r (Signal f) =  Signal (\rate ->
    let xs = f rate
        rate' = fromIntegral rate
        n = round (a * rate')
        m = round (d * rate')
        o = round (r * rate')
        p = length xs'' - o
        xs' = drop n xs
        xs'' = drop m xs'
        xs''' = drop p xs''
        as = zipWith (*) [0, 1/(fromIntegral n)..] (take n xs)
        ds = zipWith (*) [1, 1-(1-s)/(fromIntegral m)..] (take m xs')
        ss = zipWith (*) (repeat s) (take p xs'')
        rs = zipWith (*) [s, s-s/(fromIntegral o)..] xs'''
    in as ++ ds ++ ss ++ rs)

-- | (+>)
-- Sequence two signals.
infixl 1 +>
(+>) :: (Ord a, Num a) => Signal a -> (Phase a -> Signal a) -> Signal a
(Signal f) +> g = Signal (\rate ->
    let xs = f rate
        b = if null xs then 0 else last xs
        a = if null (init xs) then 0 else last (init xs)
        Signal h = g (b, compare a b)
    in xs ++ h rate)

-- | join
-- Sequence a list fo signals.
join :: (Ord a, Num a) => [Phase a -> Signal a] -> (Phase a -> Signal a)
join [] _ = mempty
join (x:xs) phi = x phi +> join xs

-- | start
--
start :: (Ord a, Num a) => (Phase a -> Signal a) -> Signal a
start = (+>) mempty


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


-- | FORMAT AND EXPORT FUNCTIONS

-- | toWave
-- Generate a wave from a signal, that can be stored as a .wav file by the WAVE
-- package:
--
-- $ cabal install WAVE
--
toWave :: Rate -> Signal Double -> WAVE
toWave rate (Signal f) = let samples = f rate
                             header = WAVEHeader 1 rate 32 (Just $ length samples)
                             body = map (pure . doubleToSample) samples
                         in WAVE header body


-- | AUXILIAR OPERATIONS (DO NOT EXPORT)

-- | dropSamples
dropSamples :: (Eq a, Ord a) => Phase a -> [a] -> [a]
dropSamples (phi,ord) (x:y:xs) = if x == y then xs else case ord of
    LT -> if x <= phi && y > phi then y:xs else dropSamples (phi,ord) (y:xs)
    GT -> if x > phi && y <= phi then y:xs else dropSamples (phi,ord) (y:xs)
    EQ -> if x == phi || x < phi && y > phi || x > phi && y < phi
          then y:xs
          else dropSamples (phi,ord) (y:xs)