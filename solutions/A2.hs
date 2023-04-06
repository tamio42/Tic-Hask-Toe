{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module A2 where

import A1
import Data.List (intercalate)
import Text.Read (readMaybe)
import Data.Char (digitToInt)
import Data.Char (toUpper)

-- *** Assignment 2-1 *** --

-- Q#01
promptPlayer :: Player -> String
promptPlayer p = concat ["Player ", show p, "'s turn: enter a row and column position (ex. A1)"]

-- Q#02
_RANGE_ :: [Int]
_RANGE_ = [0..(_SIZE_ - 1)]

-- Q#03
isDigit :: Char -> Bool
isDigit c = c `elem` ['0'..'9']

readDigit :: Char -> Int
readDigit c
 | isDigit c        = digitToInt c
 | toUpper c == 'A' = 0
 | toUpper c == 'B' = 1
 | toUpper c == 'C' = 2
 | otherwise        = -1

-- Q#04
_EMPTY_ROW_ :: Row
_EMPTY_ROW_ = replicate _SIZE_ E

_EMPTY_BOARD_ :: Board
_EMPTY_BOARD_ = [_EMPTY_ROW_, _EMPTY_ROW_, _EMPTY_ROW_]

-- Q#05
isTied :: Board -> Bool
isTied b =  b /= _EMPTY_BOARD_

_TIED_BOARD_ :: Board
_TIED_BOARD_ = [
        [X, O, O]
      , [O, X, X]
      , [O, X, O]
      ]

tieb :: Board
tieb = _TIED_BOARD_
-- Q#06
indexRowStrings :: [String] -> [(Char,String)]
indexRowStrings s = zip ['A'..] s

-- Q#07
formatLine :: [String] -> String
formatLine s = intercalate _SEP_ s

-- *** Assignment 2-2 *** --

-- Q#08
isMoveInBounds :: Move -> Bool
isMoveInBounds (a, b)
 | a < 0 || a > length _RANGE_ -1 = False
 | b < 0 || b > length _RANGE_ -1 = False
 | otherwise = True

-- Q#09

stringToMove :: String -> Move
stringToMove s =
 let a = readDigit (head s)
     b = readDigit (last s)
 in if isMoveInBounds (a,b) && length s < 3 then (a,b) else _INVALID_MOVE_

-- Q#10
replaceSquareInRow :: Player -> Int -> Row -> Row
replaceSquareInRow p i r
 | null r    = []
 | i < 0     = r
 | i > 2     = r
 | i == 0    = p : tail r
 | i == 1    = [head r, p, last r]
 | i == 2    = [head r, head r, p]
 | otherwise = r

rsX :: Int -> Row -> Row
rsX i r = replaceSquareInRow X i r

rsO :: Int -> Row -> Row
rsO i r = replaceSquareInRow O i r

rex :: (Player, Int, Row)
rex = (X, 1, ooo)

reo :: (Player, Int, Row)
reo = (O, 1, xxx)

xxx :: Row
xxx = [X,X,X]

ooo :: Row
ooo = [O,O,O]

eee :: Row
eee = [E,E,E]

xoe :: Row
xoe = [X,O,E]

t1 :: Row -> Row
t1 r = take 1 r

t2 :: Row -> Row
t2 r = take 2 r

rats :: String -> String
rats (x:xs) = reverse xs ++ [x]