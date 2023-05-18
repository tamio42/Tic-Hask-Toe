module Main where

import A1
import A2
import A3
import A4
import A5
import Sandbox
import Control.Concurrent (yield)

main :: IO ()
main = firstPlayerDo >>= (\p -> if show p == "X" then playDo eb X else playDo eb O)