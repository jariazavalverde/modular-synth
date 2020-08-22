module Data.Note(
    -- data types
    Note(..), NoteValue, Pitch, Tempo,
    -- operations
    duration,
    noteToSignal, noteToSignal0, notesToSignal, notesToSignal0,
    -- common note pitches
    c, d, e, f, g, a, b,
    c',
    csharp, dsharp, fsharp, gsharp, asharp,
    dflat, eflat, gflat, aflat, bflat,
    -- common note values
    n1, n4, n8, n16, n32, n64, n128, n256, whole, semibreve, half, minim,
    quarter, crotchet, eighth, quaver, sixteenth, semiquaver, thirtySecond,
    demisemiquaver, sixtyFourth, hemidemisemiquaver, semidemisemiquaver,
    hundredTwentyEighth, semihemidemisemiquaver, quasihemidemisemiquaver,
    twoHundredFiftySixth, demisemihemidemisemiquaver,
    dotted, duplet, triplet
) where


import Data.Signal(Signal(..))


-- | DATA TYPES

-- | Tempo (in bpm)
-- The tempo is the speed or pace of a given piece.
type Tempo = Int

-- | Note value
-- A note value indicates the relative duration of a note.
type NoteValue = Double

-- | Pitch (in Hz)
-- 
type Pitch = Double

-- | Note
-- A note is a symbol denoting a musical sound.
-- Notes can represent the pitch and duration of a sound in musical notation.
data Note = Note NoteValue Pitch
          | Dotted Int Note
          | Duplet Note Note
          | Triplet Note Note Note
          deriving (Read, Show, Eq, Ord)


-- | OPERATIONS

-- | duration 
-- Calculate the duration of a note.
duration :: Tempo -> Note -> Double
duration tempo (Note value _) = value * (60 / fromIntegral tempo)
duration tempo (Dotted n note) = let d = duration tempo note
                                 in d + d * ((2^n - 1) / 2^n)
duration tempo (Duplet note _) = 3 * duration tempo note
duration tempo (Triplet note _ _) = 2 * duration tempo note

-- | noteToSignal
-- Generate a signal from a note.
noteToSignal :: Double -> Tempo -> Note -> Signal Double
noteToSignal phi tempo note@(Note value pitch) = Signal (\t ->
    if t >= phi && t <= phi + duration tempo note
        then pitch
        else 0)

noteToSignal0 :: Tempo -> Note -> Signal Double
noteToSignal0 = noteToSignal 0

-- | notesToSignal
-- Generate a signal from a list of notes.
notesToSignal :: Double -> Tempo -> [Note] -> Signal Double
notesToSignal _ _ [] = Signal $ const 0
notesToSignal phi tempo ((Note value pitch):xs) = Signal (\t ->
    let phi' = phi + value * (60 / fromIntegral tempo)
    in if t >= phi && t <= phi'
           then pitch
           else runSignal (notesToSignal phi' tempo xs) t)

notesToSignal0 :: Tempo -> [Note] -> Signal Double
notesToSignal0 = notesToSignal 0


-- | COMMON NOTE PITCHES

c, d, e, f, g, a, b :: Pitch
c = 261.63
d = 293.66
e = 329.63
f = 349.23
g = 392.00
a = 440.00
b = 493.88

c' :: Pitch
c' = 523.251

csharp, dsharp, fsharp, gsharp, asharp :: Pitch
csharp = 277.18
dsharp = 311.13
fsharp = 369.99
gsharp = 415.30
asharp = 466.16

dflat, eflat, gflat, aflat, bflat :: Pitch
dflat = 277.18
eflat = 311.13
gflat = 369.99
aflat = 415.30
bflat = 466.16


-- | COMMON NOTE VALUES

-- | 4/4
n1, whole, semibreve :: Pitch -> Note
n1 = Note 1
whole = Note 1
semibreve = Note 1

-- | 2/4
n2, half, minim :: Pitch -> Note
n2 = Note (1/2)
half = Note (1/2)
minim = Note (1/2)

-- | 1/4
n4, quarter, crotchet :: Pitch -> Note
n4 = Note (1/4)
quarter = Note (1/4)
crotchet = Note (1/4)

-- | 1/8
n8, eighth, quaver :: Pitch -> Note
n8 = Note (1/8)
eighth = Note (1/8)
quaver = Note (1/8)

-- | 1/16
n16, sixteenth, semiquaver :: Pitch -> Note
n16 = Note (1/16)
sixteenth = Note (1/16)
semiquaver = Note (1/16)

-- | 1/32
n32, thirtySecond, demisemiquaver :: Pitch -> Note
n32 = Note (1/32)
thirtySecond = Note (1/32)
demisemiquaver = Note (1/32)

-- | 1/64
n64, sixtyFourth, hemidemisemiquaver, semidemisemiquaver :: Pitch -> Note
n64 = Note (1/64)
sixtyFourth = Note (1/64)
hemidemisemiquaver = Note (1/64)
semidemisemiquaver = Note (1/64)

-- | 1/128
n128, hundredTwentyEighth, semihemidemisemiquaver, quasihemidemisemiquaver :: Pitch -> Note
n128 = Note (1/128)
hundredTwentyEighth = Note (1/128)
semihemidemisemiquaver = Note (1/128)
quasihemidemisemiquaver = Note (1/128)

-- | 1/256
n256, twoHundredFiftySixth, demisemihemidemisemiquaver :: Pitch -> Note
n256 = Note (1/256)
twoHundredFiftySixth = Note (1/256)
demisemihemidemisemiquaver = Note (1/256)

dotted :: Int -> Note -> Note
dotted = Dotted

-- | Extra-metric groupings
duplet :: (Pitch -> Note) -> Pitch -> Pitch -> Note
duplet f a b = Duplet (f a) (f b)

triplet :: (Pitch -> Note) -> Pitch -> Pitch -> Pitch -> Note
triplet f a b c = Triplet (f a) (f b) (f c)