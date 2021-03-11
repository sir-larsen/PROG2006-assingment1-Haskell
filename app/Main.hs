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


getBoardIndex :: String -> Maybe Int
getBoardIndex "1" = Just 0
getBoardIndex "2" = Just 1
getBoardIndex "3" = Just 2
getBoardIndex "4" = Just 3
getBoardIndex "5" = Just 4
getBoardIndex "6" = Just 5
getBoardIndex "7" = Just 6
getBoardIndex "8" = Just 7
getBoardIndex "9" = Just 8
getBoardIndex _    = Nothing

getBoardIndex2 :: String -> Int
getBoardIndex2 "1" = 0
getBoardIndex2 "2" = 1
getBoardIndex2 "3" = 2
getBoardIndex2 "4" = 3
getBoardIndex2 "5" = 4
getBoardIndex2 "6" = 5
getBoardIndex2 "7" = 6
getBoardIndex2 "8" = 7
getBoardIndex2 "9" = 8


possibleMoves2 :: [Cell] -> [Cell]
possibleMoves2 board = tail board 

{-|possibleMoves :: [Cell] -> Int -> [Int] -> [Int]
--possibleMoves board = [x | x <- board, x == Blank]
--possibleMoves board = [ | x <- board, x == Blank] -- MÅ SENDE MED 0
--possibleMoves = map (\a -> 1+1) board
possibleMoves [] n xs = xs ++ [0]
possibleMoves board n xs
    | head board == Blank = do
        xs ++ [(n+1)]
        possibleMoves (tail board) (n+1) xs
    | otherwise = possibleMoves (tail board) (n+1) xs
-}
checkIsFree :: [Cell] -> Int -> Maybe Int
checkIsFree board i = if board !! i == Blank then Just i else Nothing

possibleMoves3 board = map (\x -> checkIsFree board x) [0..8]


-- BRUKES SAMMED MED POSSIBLEMOVES4
checkIsFree2 :: [Cell] -> Int -> Bool
checkIsFree2 board i = if board !! i == Blank then True else False


-- BRUKES SAMMEN MED CHECKISFREE2
possibleMoves4 :: [Cell] -> [Int]
possibleMoves4 board = filter (\x -> checkIsFree2 board x) [0..8]




assignCell :: String -> Symbol -> [Cell] -> [Cell]
assignCell location symbol board = do
    let Just x = getBoardIndex location
    ((take x board) ++ [Occupied symbol] ++ (drop (x+1) board))

gameLoopHH :: Bool -> Symbol -> [Cell] -> IO ()
gameLoopHH play sym board = 
    if play == True then do
        renderBoard board
        cell <- getLine
        let index = getBoardIndex2 cell
        let check = checkIsFree2 board index
        if check == True then do
            let newBoard = assignCell cell sym board
            --Check winner
            --Check draw
            gameLoopHH False O newBoard
        else do
            putStrLn "GAME OVER X LOST"
            return ()
    else do
        renderBoard board
        cell <- getLine
        let index = getBoardIndex2 cell
        let check = checkIsFree2 board index
        if check == True then do
            let newBoard = assignCell cell sym board
            --Check winner
            --Check draw
            gameLoopHH True X newBoard
        else do
            putStrLn "GAME OVER O LOST"
            return ()

    












main :: IO ()
main = do
    someFunc
    let newBoard = replicate 9 Blank
    --renderBoard newBoard
    gameLoopHH True X newBoard
    --putStrLn "\n"
    --let x = assignCell "6" X newBoard
    --renderBoard x
    --let listInts = []

