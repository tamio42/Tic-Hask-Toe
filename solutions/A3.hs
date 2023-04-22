module A3 where

import A1
import A2

import Data.List (transpose)
import Numeric (showInt)
import Text.Read (Lexeme(String, Char))
import Foreign (toBool)
import GHC.Arr (accum)
-- import Sandbox (b)

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
showSquares (x:xs) = showSquare x : showSquares xs

-- Q#03
formatRows :: [Row] -> [String]
formatRows [] = []
formatRows (r:rs) = _HEADER_ r : formatRows rs

-- Q#04
isColEmpty :: Row -> Int -> Bool
isColEmpty r c
 | c == 0 && head r == E         = True
 | c == 1 && head (tail r) == E  = True
 | c == 2 && last r == E         = True
 | otherwise                     = False

-- Q#05
dropFirstCol :: Board -> Board
dropFirstCol []     = []
dropFirstCol (b:bs) = tail b : dropFirstCol bs

dropLastCol :: Board -> Board
dropLastCol []     = []
dropLastCol (b:bs) = init b : dropLastCol bs

-- Q#06
getDiag1 :: Board -> Line
getDiag1 [] = []
getDiag1 (b:bs) = head b : getDiag1 (dropFirstCol bs)

getDiag2 :: Board -> Line
getDiag2 [] = []
getDiag2 (b:bs) = last b : getDiag2 (dropLastCol bs)

getAllLines :: Board -> [Row]
getAllLines b = [head b, head (tail b), last b, 
         head c, head (tail c), last c, 
         getDiag1 b, getDiag2 b]
 where c = transpose b

-- _TIED_BOARD_ = [
--         [E, O, O]
--       , [O, E, X]
--       , [O, X, E]
--       ]

-- *** Assignment 3-2 ***

-- Q#07

putSquare :: Player -> Board -> Move -> Board
putSquare pl b (row, col) = replaceElem row b $ replaceElem col (b !! row) pl
  where
    replaceElem :: Int -> [a] -> a -> [a]
    replaceElem _ [] _ = []
    replaceElem index olds new = let (l, r) = splitAt index olds in l ++ (new : tail r)

-- putSquare :: Player -> Board -> Move -> Board
-- putSquare _ [] _ = []
-- putSquare p b m
--  | r == 0 = [replaceSquareInRow p1 c (head b), head (tail b), last b]
--  | r == 1 = [head (tail b), replaceSquareInRow p1 c (head b), last b]
--  | r == 2 = [head (tail b), last b, replaceSquareInRow p1 c (head b)]
--  | otherwise = []
--  where 
--   r = fst m
--   c = snd m
--   p1 = p

-- Q#08
prependRowIndicesWorker :: [(Char,String)] -> [String]
prependRowIndicesWorker [] = []
prependRowIndicesWorker ((c,s) : xs) = (c : s) : prependRowIndicesWorker xs

prependRowIndices :: [String] -> [String]
prependRowIndices = prependRowIndicesWorker . indexRowStrings

-- Q#09
isWinningLine :: Player -> Line -> Bool
isWinningLine p l = go False l where
  go :: Bool -> Line -> Bool
  go acc [] = acc
  go acc (x:xs) = (x == p) && go True xs 

-- Q#10
isValidMove :: Board -> Move -> Bool
isValidMove [] _ = False
isValidMove b m  = isMoveInBounds m && isColEmpty r c
 where
    r = b !! fst m
    c = snd m