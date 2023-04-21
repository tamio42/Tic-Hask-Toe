module A4 where

import A1
import A2
import A3 hiding (
  _HEADER_,
  showSquares,
  dropFirstCol,
  dropLastCol,
  formatRows,
  isWinningLine,
  prependRowIndices
  )
import Data.List (intercalate)
import Sandbox (m)

-- *** Assignment 4-1 *** --

-- Q#01
_HEADER_ :: Row -> String
_HEADER_ r = formatLine $ [" "] ++ map show r ++ [" "]

-- Q#02
showSquares :: [Square] -> [String]
showSquares [] = []
showSquares xs = map show xs

-- Q#03
dropFirstCol :: Board -> Board
dropFirstCol []     = []
dropFirstCol b = map (drop 1) b

-- Q#04
dropLastCol :: Board -> Board
dropLastCol []     = []
dropLastCol b = map (take 2) b

--Q#05
formatRows :: [Row] -> [String]
formatRows []     = []
formatRows rs = map _HEADER_ rs

-- Q#06
isWinningLine_ :: Player -> Line -> Bool
isWinningLine_ p l
 | null (filter (== p) l)     = False
 | filter (== p) l == [E,E,E] = False
 | otherwise                  = True

-- *** Assignment 4-2 *** --

-- Q#07
isWinningLine :: Player -> Line -> Bool
isWinningLine p l =
  p /= E        &&
  length l == 3 &&
  foldr (\x acc -> acc && x == p) True l

-- Q#08
hasWon :: Player -> Board -> Bool
hasWon _ [] = False 
hasWon p b = 
  let bs = getAllLines b
  in foldr (\line acc -> p /= E && (acc || line == [p, p, p])) False bs     

wbx :: Board
wbx = [[X, O, O],
       [O, X, O],
       [O, O, X]]

wbo :: Board
wbo = [ [O, X, O]
              , [X, X, O]
              , [X, O, O]
              ]

pb :: Board
pb = [ [E, X, O]
              , [X, E, O]
              , [X, O, E]
              ]              

-- Q#09
getGameState :: Board -> GameState
getGameState b
 | hasWon X b        = XWins
 | hasWon O b        = OWins
 | E `elem` concat b = Playing
 | otherwise         = Tie

playMove :: Player -> Board -> Move -> (GameState, Board)
playMove p b m = if isValidMove b m then (getGameState (putSquare p b m), putSquare p b m) 
                                    else (InvalidMove, b)


-- showGameState :: GameState -> String
-- showGameState gs = case gs of
--  XWins   -> "XWins"
--  OWins   -> "OWins"
--  Tie     -> "TieGame"
--  Playing -> "Playing"

-- Q#10
prependRowIndices :: [String] -> [String]
prependRowIndices [] = []
prependRowIndices s  = zipWith (:) ['A'..] s

-- Q#11
formatBoard :: Board -> String
formatBoard b =   intercalate "\n" $ "  _|_0_|_1_|_2_|_" : prependRowIndices (formatRows tb)