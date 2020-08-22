module Data.VCO(
    -- data types
    VCO(..),
    -- inputs (inVoltageVCO)
    -- outputs
    outSineVCO, outSquareVCO, outTriangleVCO, outSawVCO
) where


import Data.Signal(Signal(..))


-- | VCO
-- A voltage-controlled oscillator (VCO) is an electronic oscillator whose
-- oscillation frequency is controlled by a voltage input.
data VCO a = VCO {
    -- Multiplier for frequency.
    inFrequencyVCO :: a,
    -- The applied input voltage determines the instantaneous oscillation 
    -- frequency.
    inVoltageVCO :: Signal a
}


-- | OUTPUTS
-- VCO has 4 outputs, 1 for each type of wave:
--
-- * sine (outSineVCO),
outSineVCO :: (Num a, Floating a) => VCO a -> Signal a
outSineVCO = sine <$> inFrequencyVCO <*> inVoltageVCO
--
-- * square (outSquareVCO),
outSquareVCO :: (Num a, Floating a) => VCO a -> Signal a
outSquareVCO = square <$> inFrequencyVCO <*> inVoltageVCO
--
-- * triangle (outTriangleVCO),
outTriangleVCO :: (Num a, Floating a) => VCO a -> Signal a
outTriangleVCO = triangle <$> inFrequencyVCO <*> inVoltageVCO
--
-- * and sawtooth (outSawVCO).
outSawVCO :: (Num a, Floating a, RealFrac a) => VCO a -> Signal a
outSawVCO = saw <$> inFrequencyVCO <*> inVoltageVCO


-- | SIGNALS

-- | sine
-- A sine wave or sinusoid is a mathematical curve that describes a smooth
-- periodic oscillation. A sine wave is a continuous wave.
-- Its form as a function of time t is: 
--
-- y(t) = A * sin(2*pi*f*t + phi)
--
-- where A is the amplitude, f is the frecuency and phi is the phase.
sine :: (Num a, Floating a) => a -> Signal a -> Signal a
sine p (Signal f) = Signal (\t -> sin (2*pi*f(t)*t*p))

-- | square
-- A square wave is a non-sinusoidal periodic waveform in which the amplitude
-- alternates at a steady frequency between fixed minimum and maximum values,
-- with the same duration at minimum and maximum.
-- Its form as a function of time t is: 
--
-- y(t) = A * sgn(sin(2*pi*f*t + phi))
--
-- where A is the amplitude, f is the frecuency and phi is the phase.
square :: (Num a, Floating a) => a -> Signal a -> Signal a
square p (Signal f) = Signal (\t -> signum (sin (2*pi*f(t)*t*p)))

-- | triangle
-- A triangle wave is a non-sinusoidal waveform named for its triangular shape.
-- It is a periodic, piecewise linear, continuous real function. 
-- Its form as a function of time t is: 
--
-- y(t) = 2*A / pi * asin(sin(2*pi*f*t + phi))
--
-- where A is the amplitude, f is the frecuency and phi is the phase.
triangle :: (Num a, Floating a) => a -> Signal a -> Signal a
triangle p (Signal f) = Signal (\t -> (2 / pi) * asin (sin (2*pi*f(t)*t*p)))

-- | saw
-- The sawtooth wave is a kind of non-sinusoidal waveform. The convention is
-- that a sawtooth wave ramps upward and then sharply drops. In a reverse
-- (or inverse) sawtooth wave, the wave ramps downward and then sharply rises.
-- Its form as a function of time t is: 
--
-- y(t) = -2*A * ((t+phi)*f - floor(0.5+(t+phi)*f))
--
-- where A is the amplitude, f is the frecuency and phi is the phase.
saw :: (Num a, Floating a, RealFrac a) => a -> Signal a -> Signal a
saw p (Signal f) = Signal (\t -> 2*t*f(t)*p - fromIntegral (floor (0.5+t*f(t)*p)))