module AirType.Helpers where

import AirType.Types
import Data.Char

fromString :: String -> Maybe [Input]
fromString = mapM fromChar

fromInputs :: [Input] -> [[Char]]
fromInputs = fmap fromInput

fromInput :: Input -> [Char]
fromInput L1 = [' ']
fromInput L2 = ['v', 'f', 'r', 'g', 't', 'b']
fromInput L3 = ['c', 'd', 'e']
fromInput L4 = ['x', 's', 'w']
fromInput L5 = ['z', 'a', 'q', '\t']
fromInput R1 = [' ']
fromInput R2 = ['m', 'n', 'j', 'h', 'u', 'y']
fromInput R3 = ['k', 'i', ',']
fromInput R4 = ['.', 'l', 'o']
fromInput R5 = [';', 'p', '/', '\'', '[', ']', '\\']

fromChar :: Char -> Maybe Input
fromChar c = case toLower c of
    'a' -> Just L5
    'b' -> Just L2
    'c' -> Just L3
    'd' -> Just L3
    'e' -> Just L3
    'f' -> Just L2
    'g' -> Just L2
    'h' -> Just R2
    'i' -> Just R3
    'j' -> Just R2
    'k' -> Just R3
    'l' -> Just R4
    'm' -> Just R2
    'n' -> Just R2
    'o' -> Just R4
    'p' -> Just R5
    'q' -> Just L5
    'r' -> Just L2
    's' -> Just L4
    't' -> Just L2
    'u' -> Just R2
    'v' -> Just L2
    'w' -> Just L4
    'x' -> Just L4
    'y' -> Just R2
    'z' -> Just L5
    ',' -> Just R3
    '.' -> Just R4
    '/' -> Just R5
    '\''-> Just R5
    '[' -> Just R5
    ']' -> Just R5
    '\\'-> Just R5
    '\n'-> Just R5
    '\t'-> Just L5
    ' ' -> Just L1
    _   -> Nothing
