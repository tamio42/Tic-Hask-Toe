module A1 where
import Data.Char
import Prelude
import GHC.Generics (S)

-- *** Assignment 1-1 *** --

-- Q#01
_SIZE_ :: Int
_SIZE_ = 3

-- Q#02
_DISPLAY_LOGO_ :: Bool
_DISPLAY_LOGO_ = True

-- Q#03
cRow :: Char -> Int
cRow c = fromEnum (toUpper c) - 65

-- Q#04
_INVALID_MOVE_ :: (Int, Int)
_INVALID_MOVE_ = (-1, -1)

-- Q#05
_SEP_ :: [Char]
_SEP_ = ['_','|','_']

-- *** Assignment 1-2 *** --

-- Q#06
data Square = X | O | E deriving (Show, Eq)


-- Q#07
data GameState = Playing | Tie | XWins | OWins | InvalidMove deriving (Show, Eq)
 


-- Q#08


type Player = Square
type Row    = [Square]
type Line   = [Square]
type Board  = [Row]
type Move   = (Int, Int)

_Player1 :: Player
_Player1 = X

_Row :: Row
_Row = [X, O, E]

_Line :: Line
_Line = [X, O, E]

-- Q#09
getFirstPlayer :: Bool -> Player
getFirstPlayer p
  | p = X
  | otherwise = O

-- Q#10
showGameState :: GameState -> String
showGameState gs = case gs of
 XWins       -> "XWins"
 OWins       -> "OWins"
 Tie         -> "TieGame"
 Playing     -> "Playing"
 InvalidMove -> "Invalid Move"

-- Q#11
switchPlayer :: Player -> Player
switchPlayer p = case p of
 X -> O
 O -> X
 E -> E


-- Q#12
showSquare :: Square -> String
showSquare s = case s of
 X -> "X"
 O -> "O"
 E -> "_"

winner :: Player -> String
winner p = show p ++ " WINS THE GAME! \n"

tieGame :: String
tieGame = "TIE GAME! \n"