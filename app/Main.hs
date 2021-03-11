module Main where

import Lib

import System.Random
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

--getMove :: [Cell] -> IO ()
getMove board = do
    let moves = possibleMoves4 board
    num <- randomRIO (0, 8) :: IO Int
    --return num
    --putStrLn("randnum" ++ show num)
    let x = elem num moves
    if x == True then return num
    else do
        getMove board

getMove2 board = do
    gen <- getStdGen
    let (rNum, _) = randomR (0,8) gen :: (Int, StdGen)
    let moves = possibleMoves4 board
    let x = elem rNum moves
    newStdGen
    if x == True then return rNum
    else do
        newStdGen
        getMove2 board


assignCell :: String -> Symbol -> [Cell] -> [Cell]
assignCell location symbol board = do
    let Just x = getBoardIndex location
    ((take x board) ++ [Occupied symbol] ++ (drop (x+1) board))

assignCell2 :: Int -> Symbol -> [Cell] -> [Cell]
assignCell2 location symbol board = ((take location board) ++ [Occupied symbol] ++ (drop (location+1) board))

getNum = do
    gen <- getStdGen
    let (rNum, _) = randomR (0,8) gen :: (Int, StdGen)
    return rNum


--getRegInt :: IO Int -> Int
--getRegInt 0 = 0

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

gameLoopHM :: Bool -> Symbol -> [Cell] -> IO ()
gameLoopHM play sym board = do
    if play == True then do
        renderBoard board
        cell <- getLine
        let index = getBoardIndex2 cell
        let check = checkIsFree2 board index
        putStrLn(show check)
        if check == True then do
            let newBoard = assignCell cell sym board
            --Check winner
            --Check draw
            if length (possibleMoves4 newBoard) == 0 then do
                renderBoard newBoard
                putStrLn "Alle brikker spilt"
                return ()
            else do
                gameLoopHM False O newBoard
        else do
            putStrLn "GAME OVER X LOST"
            return ()
    else do
        --let loop = do
        num <- getStdRandom $ randomR (0, 8 :: Int)
        let moves = possibleMoves4 board
        let x = elem num moves

        if x == True then do
            let newBoard = assignCell2 num sym board
            gameLoopHM True X newBoard
        else do gameLoopHM False O board
        --loop

main :: IO ()
main = do
    someFunc
    let newBoard = replicate 9 Blank
    --let y = getNum
    --let z = getNum
    --let h = getNum

    num <- randomRIO (0, 8) :: IO Int
    putStrLn("randnum" ++ show num)

    num2 <- randomRIO (0, 8) :: IO Int
    putStrLn("randnum" ++ show num2)

    num3 <- randomRIO (0, 8) :: IO Int
    putStrLn("randnum" ++ show num3)

    let x = assignCell2 num X newBoard
    --renderBoard x
    --putStrLn " "

    --let f = assignCell2 num2 X x
    --renderBoard f

    --let h = getMove f
    --let m = getRegInt h

    let h = getMove2 newBoard
    let f = getMove2 newBoard
    --print f

    --renderBoard newBoard
    gameLoopHM True X newBoard
    --putStrLn "\n"
    --let x = assignCell "6" X newBoard
    --renderBoard x
    --let listInts = []

