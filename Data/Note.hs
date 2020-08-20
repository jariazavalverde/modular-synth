module Data.Note(
    -- data types
    Note(..), NoteValue, Tempo,
    -- operations
    noteToSignal,
    -- common note pitches
    c, d, e, f, g, a, b,
    csharp, dsharp, fsharp, gsharp, asharp,
    dflat, eflat, gflat, aflat, bflat,
    -- common note values
    n1, n4, n8, n16, n32, n64, n128, n256, whole, semibreve, half, minim,
    quarter, crotchet, eighth, quaver, sixteenth, semiquaver, thirtySecond,
    demisemiquaver, sixtyFourth, hemidemisemiquaver, semidemisemiquaver,
    hundredTwentyEighth, semihemidemisemiquaver, quasihemidemisemiquaver,
    twoHundredFiftySixth, demisemihemidemisemiquaver,
    duplet, triplet
) where


import Data.Signal(Signal(..), Frecuency, Phase, time, time', (+>))


-- | DATA TYPES

-- | Tempo (in bpm)
-- The tempo is the speed or pace of a given piece.
type Tempo = Int

-- | NoteValue
-- A note value indicates the relative duration of a note.
type NoteValue = Double

-- | Note
-- A note is a symbol denoting a musical sound.
-- Notes can represent the pitch and duration of a sound in musical notation.
data Note = Note NoteValue Frecuency
          | Dotted Int Note
          | Duplet Note Note
          | Triplet Note Note Note
    deriving (Read, Show, Eq, Ord)


-- | OPERATIONS

-- | noteToSignal
-- Given the tempo (in bps) and a signal constructor, generates a signal from
-- a note.
noteToSignal :: Note -> Tempo -> (Frecuency -> Phase Double -> Signal Double)
    -> Phase Double -> Signal Double
noteToSignal (Note v f) t u phi =
    time (v * fromIntegral t / 60) (u f phi)
noteToSignal (Dotted n (Note v f)) t u phi =
    let v' = v + v * ((2^n - 1) / 2^n)
    in noteToSignal (Note v' f) t u phi
noteToSignal (Duplet a b) t u phi =
    time' (3/2) (noteToSignal a t u phi) +>
    time' (3/2) . noteToSignal b t u
noteToSignal (Triplet a b c) t u phi =
    time' (2/3) (noteToSignal a t u phi) +>
    time' (2/3) . noteToSignal b t u +>
    time' (2/3) . noteToSignal c t u



-- | COMMON NOTE PITCHES

c, d, e, f, g, a, b :: Frecuency
c = 261.63
d = 293.66
e = 329.63
f = 349.23
g = 392.00
a = 440.00
b = 493.88

csharp, dsharp, fsharp, gsharp, asharp :: Frecuency
csharp = 277.18
dsharp = 311.13
fsharp = 369.99
gsharp = 415.30
asharp = 466.16

dflat, eflat, gflat, aflat, bflat :: Frecuency
dflat = 277.18
eflat = 311.13
gflat = 369.99
aflat = 415.30
bflat = 466.16


-- | COMMON NOTE VALUES

-- | 4/4
n1, whole, semibreve :: Frecuency -> Note
n1 = Note 1
whole = Note 1
semibreve = Note 1

-- | 2/4
n2, half, minim :: Frecuency -> Note
n2 = Note (1/2)
half = Note (1/2)
minim = Note (1/2)

-- | 1/4
n4, quarter, crotchet :: Frecuency -> Note
n4 = Note (1/4)
quarter = Note (1/4)
crotchet = Note (1/4)

-- | 1/8
n8, eighth, quaver :: Frecuency -> Note
n8 = Note (1/8)
eighth = Note (1/8)
quaver = Note (1/8)

-- | 1/16
n16, sixteenth, semiquaver :: Frecuency -> Note
n16 = Note (1/16)
sixteenth = Note (1/16)
semiquaver = Note (1/16)

-- | 1/32
n32, thirtySecond, demisemiquaver :: Frecuency -> Note
n32 = Note (1/32)
thirtySecond = Note (1/32)
demisemiquaver = Note (1/32)

-- | 1/64
n64, sixtyFourth, hemidemisemiquaver, semidemisemiquaver :: Frecuency -> Note
n64 = Note (1/64)
sixtyFourth = Note (1/64)
hemidemisemiquaver = Note (1/64)
semidemisemiquaver = Note (1/64)

-- | 1/128
n128, hundredTwentyEighth, semihemidemisemiquaver, quasihemidemisemiquaver :: Frecuency -> Note
n128 = Note (1/128)
hundredTwentyEighth = Note (1/128)
semihemidemisemiquaver = Note (1/128)
quasihemidemisemiquaver = Note (1/128)

-- | 1/256
n256, twoHundredFiftySixth, demisemihemidemisemiquaver :: Frecuency -> Note
n256 = Note (1/256)
twoHundredFiftySixth = Note (1/256)
demisemihemidemisemiquaver = Note (1/256)

-- | Extra-metric groupings
duplet :: (Frecuency -> Note) -> Frecuency -> Frecuency -> Note
duplet f a b = Duplet (f a) (f b)

triplet :: (Frecuency -> Note) -> Frecuency -> Frecuency -> Frecuency -> Note
triplet f a b c = Triplet (f a) (f b) (f c)