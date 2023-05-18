module A5 where

import A1
import A2
import A3
import A4

import System.Random.Stateful (globalStdGen, uniformM)
import Control.Monad (when)
import Sandbox (b)
import Data.Bool (bool)


-- *** Assignment 5-1 *** --

myReplace :: String -> String
myReplace []     = []
myReplace (x:xs) = if x == 'E' then '_' : myReplace xs else x : myReplace xs

-- Q#01
printBoard :: Board -> IO ()
printBoard brd = putStrLn $ myReplace $ formatBoard brd

t42 :: String
t42 = "wakka wakka"

-- Q#02
_LOGO_PATH_ :: FilePath
_LOGO_PATH_ = "./assets/logo.txt"

_XWIN_PATH_ :: FilePath
_XWIN_PATH_ = "./assets/xwins.txt"

_OWIN_PATH_ :: FilePath
_OWIN_PATH_ = "./assets/owins.txt"

_TIEGAME_PATH_ :: FilePath
_TIEGAME_PATH_ = "./assets/tiegame.txt"

-- printLogo :: FilePath -> IO ()
-- printLogo fp = readFile fp >>= putStrLn

-- printWinner :: FilePath -> IO ()
-- printWinner fp = readFile fp >>= putStrLn >> putStrLn "\n\n"

-- Q#03
_RANDOM_BOOL_ :: IO Bool
_RANDOM_BOOL_ = uniformM globalStdGen

-- firstPlayer :: IO ()
-- firstPlayer = _RANDOM_BOOL_ >>= (\i -> print (getFirstPlayer i))
-- firstPlayer = _RANDOM_BOOL_ >>= (\i -> if getFirstPlayer i == X then putStrLn "X" else putStrLn "O")

-- -- Q#04
-- getMove :: Board -> IO Move
-- getMove b = 
--     putStrLn "Enter your move..." >>
--     getLine >>=
--     \x -> 
--         let move = stringToMove x       
--         in
--             if isValidMove b move 
--                 then return move
--                 else putStrLn "Invalid move, try again." >> getMove b

-- play :: Board -> Player -> IO ()
-- play b p = 
--     when _DISPLAY_LOGO_ $ printLogoDo _LOGO_PATH_ >>
--     printBoard b >>
--     print (promptPlayer p) >>
--     getMove b >>=
--         -- (\move -> print $ fst (playMove p b move))
--     \move ->
--         let gs   = fst (playMove p b move)
--             newB = snd (playMove p b move)
--         in case gs of
--             Playing     -> play newB (switchPlayer p)
--             Tie         -> printBoard newB
--                             >> putStrLn (concat (replicate 10 tieGame))    
--                             >> play eb (switchPlayer p)
--             XWins       -> printBoard newB
--                             -- >> putStrLn (concat (replicate 10 (winner p))) 
--                             >> printWinnerDo _XWIN_PATH_
--                             >> play eb (switchPlayer p)
--             OWins       -> printBoard newB
--                             -- >> putStrLn (concat (replicate 10 (winner p))) 
--                             >> printWinnerDo _OWIN_PATH_
--                             >> play eb (switchPlayer p)
--             InvalidMove -> putStrLn "Invalid Move. Try again..."       
--                             >> play eb (switchPlayer p)
                

-- *** Assignment 5-2 *** --

-- Q#07
printLogoDo :: FilePath -> IO ()
printLogoDo fp = do
    putStrLn "\n\n"
    file <- readFile fp
    putStrLn file

printWinnerDo :: FilePath -> IO ()
printWinnerDo fp = do
    file <- readFile fp
    putStrLn file
    putStrLn "\n"

-- Q#08
firstPlayerDo :: IO ()
firstPlayerDo = do
    bool <- _RANDOM_BOOL_
    print bool

-- Q#09
getMoveDo :: Board -> IO Move
getMoveDo b = do
    putStrLn "Enter your move..."
    strMove <- getLine
    let move = stringToMove strMove
        in
        if isValidMove b move 
            then do return move
            else do putStrLn "Invalid move, try again."
                    getMoveDo b


playDo :: Board -> Player -> IO ()
playDo b p = do
    when _DISPLAY_LOGO_ $ printLogoDo _LOGO_PATH_ 
    printBoard b 
    print $ promptPlayer p 
    move <- getMoveDo b
    let gs   = fst $ playMove p b move
        newB = snd $ playMove p b move
        in
            if      gs == Playing then do playDo newB $ switchPlayer p
            else if gs == Tie     then do printBoard newB
                                          printWinnerDo _TIEGAME_PATH_
                                          playDo eb $ switchPlayer p
            else if gs == XWins 
                                  then do printBoard newB
                                          printWinnerDo _XWIN_PATH_
                                          playDo eb $ switchPlayer p
            else if gs == OWins   then do printBoard newB
                                          printWinnerDo _OWIN_PATH_
                                          playDo eb $ switchPlayer p
                                  else do putStrLn "Invalid Move. Try again..."       
                                          playDo eb $ switchPlayer p