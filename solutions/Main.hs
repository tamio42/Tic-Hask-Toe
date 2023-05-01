module Main where

import A1
import A2
import A3
import A4
import A5
import Sandbox
import Control.Concurrent (yield)
import Sandbox (names)

main :: IO ()
main = putStrLn names >> firstPlayerDo >>= (\p -> if show p == "X" then playDo eb X else play eb O)
-- main = firstPlayer >>= (\p -> play eb p)