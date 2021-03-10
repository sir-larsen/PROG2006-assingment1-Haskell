module Main where

import Lib

import System.Environment
import Data.List

data Symbol = X | O
data Cell   = Occupied Symbol | Blank

instance Show Symbol where
    show X = "X"
    show O = "O"

instance Show Cell where
    show (Occupied X)  = "X"
    show (Occupied O)  = "O"
    show Blank         = "_"



instance Eq Cell where
    Occupied X == Occupied X = True
    Occupied O == Occupied O = True
    Blank == Blank           = True
    _ == _                   = False



renderRow :: [Cell] -> String
renderRow row = intercalate " " $ fmap show row

renderBoard :: [Cell] -> IO ()
renderBoard board = do
  putStrLn $ "# " ++ renderRow firstRow
  putStrLn $ "# " ++ renderRow secondRow
  putStrLn $ "# " ++ renderRow thirdRow
  where firstRow  = take 3 board
        secondRow = drop 3 . take 6 $ board
        thirdRow  = drop 6 board

main :: IO ()
main = do
    someFunc
    let newBoard = replicate 9 Blank
    renderBoard newBoard
