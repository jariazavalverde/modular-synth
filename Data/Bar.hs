module Data.Bar(
    -- data types
    Bar(..),
    -- operators
    barToSignals, twice
) where


import Data.Signal(Signal(..), Frecuency, Phase, (+>))
import Data.Note(Note(..), Tempo, noteToSignal)


-- | DATA TYPES

-- | TimeSignature
-- The time signature is a notational convention used in Western musical
-- notation to specify how many beats (pulses) are contained in each measure
-- (bar), and which note value is equivalent to a beat. 
data TimeSignature = Simple (Int,Int) deriving (Read, Show, Eq, Ord)

-- | Bar
-- A bar is a segment of time corresponding to a specific number of beats in
-- which each beat is represented by a particular note value.
type Bar = [Note]


-- | OPERATIONS

-- | barToSignal
-- Given the tempo (in bps) and a signal constructor, generates a signal from
-- a bar.
barToSignals :: Bar -> Tempo -> (Frecuency -> Phase Double -> Signal Double)
    -> [Phase Double -> Signal Double]
barToSignals [] _ _ = []
barToSignals (x:xs) t u = noteToSignal x t u : barToSignals xs t u

-- | twice
-- Repeat a list of bars two times.
twice :: [Bar] -> Bar
twice xs =  concat (xs ++ xs)