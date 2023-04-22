module A5 where

import A1
import A2
import A3
import A4

import System.Random.Stateful (globalStdGen, uniformM)
import Control.Monad (when)


-- *** Assignment 5-1 *** --

replace :: String -> String
replace []     = []
replace (x:xs) = if x == 'E' then '_' : replace xs else x : replace xs

-- Q#01
printBoard :: Board -> IO ()
printBoard brd = putStrLn $ replace $ formatBoard brd

t42 :: String
t42 = "wakka wakka"

-- Q#02
_LOGO_PATH_ :: FilePath
_LOGO_PATH_ = "./assets/logo.txt"

printLogo :: FilePath -> IO ()
printLogo fp = readFile fp >>= putStrLn

-- Q#03
_RANDOM_BOOL_ :: IO Bool
_RANDOM_BOOL_ = uniformM globalStdGen

-- firstPlayer :: IO ()
-- firstPlayer = return . getFirstPlayer True

-- Q#04

getMove = undefined

-- Q#05

play = undefined

-- *** Assignment 5-2 *** --

-- Q#07

printLogoDo = undefined

-- Q#08

firstPlayerDo = undefined

-- Q#09

getMoveDo = undefined

-- Q#10

playDo = undefined