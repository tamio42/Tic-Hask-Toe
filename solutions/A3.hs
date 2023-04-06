module A3 where

import A1
import A2

import Data.List (transpose)
import Numeric (showInt)

-- *** Assignment 3-1 ***

-- Q#01
showInts :: [Int] -> [String]
showInts []     = []
showInts (x:xs) = show x : showInts xs

_HEADER_ :: Row -> String
_HEADER_ r = formatLine $ [" "] ++ map show r ++ [" "]

-- Q#02
showSquares :: [Square] -> [String]
showSquares []     = []
showSquares (x:xs) = show x : showSquares xs

-- Q#03
formatRows :: [Row] -> [String]
formatRows [] = []
formatRows (r:rs) = _HEADER_ r : formatRows rs

-- Q#04
isColEmpty :: Row -> Int -> Bool
isColEmpty r c
 | c == 0 && head r == E         = True
 | c == 1 && head (tail r) == E  = True
 | c == 2 && last eee == E       = True
 | otherwise                     = False


-- | null r                        = False
-- | c < 0 || c > 2                = False

-- Q#05
dropFirstCol :: Board -> Board
dropFirstCol []     = []
dropFirstCol (b:bs) = tail b : dropFirstCol bs

dropLastCol :: Board -> Board
dropLastCol []      = []
dropLastCol (b:bs) = take 2 b : dropLastCol bs

-- Q#06
getDiag1 :: Board -> Line
getDiag1 [] = []
getDiag1 (b:bs) = head b : getDiag1 (dropFirstCol bs)

getDiag2 :: Board -> Line
getDiag2 [] = []
getDiag2 (b:bs) = last b : getDiag2 (dropLastCol bs)


getAllLines = undefined

-- _TIED_BOARD_ = [
--         [X, O, O]
--       , [O, X, X]
--       , [O, X, O]
--       ]

-- *** Assignment 3-2 ***

-- Q#07

putSquare = undefined

-- Q#08

prependRowIndices = undefined

-- Q#09

isWinningLine = undefined

-- Q#10

isValidMove = undefined