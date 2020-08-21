import Data.WAVE(putWAVEFile)
import Data.Signal
import Data.Note

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

piece = part1 ++ part1 ++ part2 ++ part3

titanic :: Signal Double
titanic = start $ makePiece 140 triangleWave [adsr 0.1 0.2 0.5 0.2] piece
-- titanic = mempty +>
--           join (map (((.) (adsr 0.1 0.2 0.5 0.2)) . (noteToSignal 140 triangleWave)) piece)

main :: IO ()
main = putWAVEFile "titanic.wav" (toWave audRate titanic)