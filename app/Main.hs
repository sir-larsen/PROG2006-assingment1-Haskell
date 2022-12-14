module Main where

import Lib
import Iofuncs
import System.Environment
import Data.List


main :: IO ()
main = do
    args <- getArgs
    let newBoard = replicate 9 Blank

    if (args /= []) then do
        if (args !! 0) == "hm" then gameLoopHM2 True X newBoard
        else if (args !! 0) == "hh" then gameLoopHH True X newBoard
        else if (args !! 0) == "mm" then gameLoopMM True X newBoard
        else if (args !! 0) == "help" then helper
        else do
            putStrLn "Something went wrong"
            return () 
    else 
        gameLoopHM2 True X newBoard
    
helper:: IO ()
helper = do
    putStrLn "Start with hm flag for human vs machine: hm"
    putStrLn "Start with hh flag for human vs human: hh"
    putStrLn "Start with mm flag for machine vs machine"
