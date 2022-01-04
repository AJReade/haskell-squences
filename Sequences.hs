module Sequences where

import Data.Char (ord, chr)

-- Returns the first argument if it is larger than the second,
-- the second argument otherwise
maxOf2 :: Int -> Int -> Int
maxOf2 x y
  | x > y     = x
  | otherwise = y

-- Returns the largest of three Ints
maxOf3 :: Int -> Int -> Int -> Int
maxOf3 x y z = maxOf2 z (maxOf2 y x)

-- | x > y && x > z = x
-- | y > x && y > z = y
-- | otherwise      = z

-- Returns True if the character represents a digit '0'..'9';
-- False otherwise
isADigit :: Char -> Bool
isADigit char
  | char >= '0' && char <= '9' = True
  | otherwise                  = False


-- Returns True if the character represents an alphabetic
-- character either in the range 'a'..'z' or in the range 'A'..'Z';
-- False otherwise
isAlpha :: Char -> Bool
isAlpha char
  | char >= 'a' && char <= 'z' = True
  | char >= 'A' && char <= 'Z' = True
  | otherwise                  = False

-- Returns the integer [0..9] corresponding to the given character.
-- Note: this is a simpler version of digitToInt in module Data.Char,
-- which does not assume the precondition.
digitToInt :: Char -> Int
-- Pre: the character is one of '0'..'9'
digitToInt char
  = ord char - ord '0'


-- Returns the upper case character corresponding to the input.
-- Uses guards by way of variety.
toUpper :: Char -> Char
toUpper char
  | char `elem` ['a'..'z']
    = chr (ord char - asciiDif)
  | otherwise = char
  where
    asciiDif = ord 'a' - ord 'A'

--
-- Sequences and series
--

-- Arithmetic sequence
arithmeticSeq :: Double -> Double -> Int -> Double
arithmeticSeq a d n
  = a + nth * d
  where
    nth = fromIntegral n

-- Geometric sequence
geometricSeq :: Double -> Double -> Int -> Double
geometricSeq a r n
  = a * r ^ nth
  where
    nth = fromIntegral n

-- Arithmetic series
arithmeticSeries :: Double -> Double -> Int -> Double
arithmeticSeries a d n
  = (nth + 1) * (a + d * nth * 0.5)
  where
    nth = fromIntegral n

-- Geometric series
geometricSeries :: Double -> Double -> Int -> Double
geometricSeries a r n
  | r == 1 = a * (nth + 1)
  | otherwise = a * ((1 - r ^ (n + 1)) / (1 - r))
  where
    nth = fromIntegral n
