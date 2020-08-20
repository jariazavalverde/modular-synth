import Data.WAVE(putWAVEFile)
import Data.Signal
import Data.Note
import Data.Bar

part1 = [
    half f, quarter f, quarter f,
    quarter e, half f, quarter f,
    quarter e, half f, quarter g,
    half a, half g,
    half f, quarter f, quarter f,
    quarter e, half f, quarter f,
    whole c]

part2 = [
    whole 0,
    whole f,
    dotted 1 (half g), quarter c,
    half c', quarter bflat, quarter a,
    half g, quarter a, quarter bflat,
    half a, quarter g, quarter f,
    quarter e, half f, quarter f,
    whole c,
    whole 0]

part3 = [
    whole f,
    dotted 1 (half g), quarter c,
    half c', quarter bflat, quarter a,
    half g, quarter a, quarter bflat,
    half a, quarter g, quarter f,
    quarter e, half f, quarter f,
    quarter e, half f, quarter g,
    half a, half g,
    whole f]

titanic :: Signal Double
titanic =
    mempty +>
    join (map ((.) (decay 0.2)) (barToSignals (part1 ++ part1 ++ part2 ++ part3) 140 sineWave))

main :: IO ()
main = putWAVEFile "titanic.wav" (toWave audRate titanic)