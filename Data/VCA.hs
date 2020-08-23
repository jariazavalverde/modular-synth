module Data.VCA(
    -- data types
    VCA(..),
    -- inputs
    {-
    inGainVCA, inVoltageVCA, inSignalVCA,
    -}
    -- outputs
    outSignalVCA,
    -- operations
    vca
) where


import Data.Signal(Signal(..))


-- | VCA
-- A voltage-controlled amplifier is an electronic amplifier that varies its
-- gain depending on a control voltage.
data VCA a = VCA {
    -- Multiplier for gain.
    inGainVCA :: a,
    -- The applied input voltage determines the instantaneous gain.
    inVoltageVCA :: Signal a,
    -- The input signal.
    inSignalVCA :: Signal a
}


-- | OUTPUTS
-- VCA has one output:
--
-- * the amplified signal.
outSignalVCA :: Num a => VCA a -> Signal a
outSignalVCA (VCA gain cv signal) = signal * cv * (Signal $ const gain)


-- | OPERATIONS

-- | vca
-- Return the amplified signal.
vca :: (Num a) => a -> Signal a -> Signal a -> Signal a
vca gain cv signal = outSignalVCA (VCA gain cv signal)