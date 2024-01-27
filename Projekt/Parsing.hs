-- Parsing.hs

module Parsing (sleashChar, periodChar, anyDigit, anyNotDigit, anyAlpha,
               anyNotAlpha, anyWhite, anyNotWhite, optionalChar,
               oneRangeRepetition, orGroupRepetitionRange,
               oneRepetition, orGroupRepetition,
               notOrGroup, range, escapeChar, pipeGroup,
               anyChar, normalChar, orGroup) where

import Data.List (elemIndex)
import Data.Char (ord, chr, isAlphaNum, isAlpha, isDigit, isSpace)
import Text.Read (readMaybe)

allASCIICharacters :: String
allAlphaNum :: String
letters :: String
digits :: String
whiteSpaces :: String

allASCIICharacters = map chr [0..127]
allAlphaNum = filter isAlphaNum allASCIICharacters
letters = filter isAlpha allASCIICharacters
digits =  filter isDigit allASCIICharacters
whiteSpaces = filter isSpace allASCIICharacters

-- Helper --
splitOnNearest :: Char -> String -> (String, String)
splitOnNearest c str =
  case elemIndex c str of
    Just index ->  let (before, after) = splitAt index str in (before, tail after)
    Nothing -> ([],[])

readDigit :: String -> String -> (Int, String)
resDigit res [] = (read res, [])
readDigit res s
  | isDigit (head s) = readDigit (res ++ [head s]) (tail s)
  | otherwise = (read res, s)

sleashChar :: Char -> String -> Bool
periodChar :: Char -> String -> Bool
anyDigit :: Char -> String -> Bool
anyNotDigit :: Char -> String -> Bool
anyAlpha :: Char -> String -> Bool
anyNotAlpha :: Char -> String -> Bool
anyWhite :: Char -> String -> Bool
anyNotWhite :: Char -> String -> Bool
optionalChar :: Char -> String -> Bool
oneRangeRepetition :: Char -> String -> Bool
orGroupRepetitionRange :: Char -> String -> Bool
oneRepetition :: Char -> String -> Bool
orGroupRepetition :: Char -> String -> Bool
notOrGroup :: Char -> String -> Bool
range :: Char -> String -> Bool

escapeChar :: Char -> Bool
pipeGroup :: Char -> Bool
anyChar :: Char -> Bool
normalChar :: Char -> Bool
orGroup :: Char -> Bool


-- Repetitions --
oneRangeRepetition f s = head s == '{' && head (snd (readDigit [] (tail s))) == '-'
orGroupRepetitionRange f s
  | not (orGroup f) = False
  | otherwise = do
    let (before, after) = splitOnNearest ']' s
    (after /= []) && (do
      oneRangeRepetition ' ' after)

oneRepetition f s = head s == '{'
orGroupRepetition f s
  | not (orGroup f) = False
  | otherwise = do
    let (before, after) = splitOnNearest ']' s
    (after /= []) && (do
      oneRepetition ' ' after)

-- Rest --
sleashChar f s = f == '\\' && head s == '\\'
periodChar f s = f == '\\' && head s == '.'
anyDigit f s = f =='\\' && head s == 'd'
anyNotDigit f s = f =='\\' && head s == 'D'
anyAlpha f s = f =='\\' && head s == 'w'
anyNotAlpha f s = f =='\\' && head s == 'W'
anyWhite f s = f =='\\' && head s == 's'
anyNotWhite f s = f =='\\' && head s == 'S'
optionalChar f s = f `elem` allAlphaNum && head s == '?'
notOrGroup f s = f =='[' && head s == '^'
range f s = f == '[' && head (tail s) == '-'

escapeChar f = f == '\\'
anyChar f = f == '.'
normalChar f = f `elem` allAlphaNum
pipeGroup f = f =='('
orGroup f = f == '['