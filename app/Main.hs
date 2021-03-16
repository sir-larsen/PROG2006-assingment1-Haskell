module Main where

import Lib
import Iofuncs
import System.Environment
import Data.List


main :: IO ()
main = do
    --let newBoard = replicate 9 Blank
    args <- getArgs
    let newBoard = replicate 9 Blank

    if (args !! 0) == "hm" then gameLoopHM2 True X newBoard
    else if (args !! 0) == "hh" then gameLoopHH True X newBoard
    else if (args !! 0) == "mm" then gameLoopMM True X newBoard
    else if (args !! 0) == "help" then helper
    else gameLoopHM2 True X newBoard
    
helper:: IO ()
helper = do
    putStrLn "Start with hm flag for human vs machine: hm"
    putStrLn "Start with hh flag for human vs human: hh"
    putStrLn "Start with mm flag for machine vs machine"
