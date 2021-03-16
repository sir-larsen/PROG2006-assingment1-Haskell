module Lib where

import System.Random
import System.Environment
import Data.List

-- | Creating my own data. Inspiration from https://github.com/nt591/haskell-playground/blob/master/random/tictactoe.hs
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


-- | RenderRow takes a [Cell] type and renders it
--
-- Examples
--
-- >>> renderRow (replicate 9 Blank)
-- "_ _ _ _ _ _ _ _ _"
--
renderRow :: [Cell] -> String
renderRow row = intercalate " " $ fmap show row


-- | Function for getting an index of the board
--
-- Examples
--
-- >>> getBoardIndex "1"
-- Just 0
--
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


-- | Same as function above, just not for maybe ints
--
-- Examples
--
-- >>> getBoardIndex2 "1"
-- 0
--
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


-- | Checks if a position on the board is free
--
-- Examples
--
-- >>> checkIsFree2 (replicate 9 Blank) 1
-- True
--
checkIsFree2 :: [Cell] -> Int -> Bool
checkIsFree2 board i = if board !! i == Blank then True else False


-- | Returns a list of avaliable moves
--
-- Examples
--
-- >>> possibleMoves4 (replicate 9 Blank)
-- [0,1,2,3,4,5,6,7,8]
--
possibleMoves4 :: [Cell] -> [Int]
possibleMoves4 board = filter (\x -> checkIsFree2 board x) [0..8]


-- | Assigns a cell in the board based on string value
--
-- Examples
--
-- >>> assignCell "1" X (replicate 9 Blank)
-- [X,_,_,_,_,_,_,_,_]
--
assignCell :: String -> Symbol -> [Cell] -> [Cell]
assignCell location symbol board = do
    let Just x = getBoardIndex location
    ((take x board) ++ [Occupied symbol] ++ (drop (x+1) board))


-- | Same as function above, only directly with int index
--
-- Examples
-- >>> assignCell2 0 X (replicate 9 Blank)
-- [X,_,_,_,_,_,_,_,_]
--
assignCell2 :: Int -> Symbol -> [Cell] -> [Cell]
assignCell2 location symbol board = ((take location board) ++ [Occupied symbol] ++ (drop (location+1) board))


-- | Checks if rotation should be added
--
-- Examples
--
-- >>> checkRot ["1", "right"]
-- True
--
checkRot :: [String] -> Bool
checkRot xs
    | length xs == 1 = False
    | otherwise      = True


-- | Checks if it should be rotated right
--
-- Examples
--
-- >>> checkRotRight "right"
-- True
--
checkRotRight :: String -> Bool
checkRotRight xs
    | xs == "right" = True
    | otherwise     = False
 

-- | Checks for win in all directions
--
-- Examples
--
-- >>> checkWin X (replicate 9 Blank)
-- False
--
checkWin :: Symbol -> [Cell] -> Bool
checkWin move board
    | board !! 0 == (Occupied move) && board !! 1 == (Occupied move) && board !! 2 == (Occupied move) = True
    | board !! 3 == (Occupied move) && board !! 4 == (Occupied move) && board !! 5 == (Occupied move) = True
    | board !! 6 == (Occupied move) && board !! 7 == (Occupied move) && board !! 8 == (Occupied move) = True
    | board !! 0 == (Occupied move) && board !! 3 == (Occupied move) && board !! 6 == (Occupied move) = True
    | board !! 1 == (Occupied move) && board !! 4 == (Occupied move) && board !! 7 == (Occupied move) = True
    | board !! 2 == (Occupied move) && board !! 5 == (Occupied move) && board !! 8 == (Occupied move) = True
    | board !! 0 == (Occupied move) && board !! 4 == (Occupied move) && board !! 8 == (Occupied move) = True
    | board !! 2 == (Occupied move) && board !! 4 == (Occupied move) && board !! 6 == (Occupied move) = True
    | otherwise = False


-- | Function for rotating the list right, and also swapping 1 and 3
--
-- Examples
--
-- >>> rotSwapRight [Blank,Occupied X,Blank,Blank,Blank,Blank,Blank,Blank, Blank]
-- [_,_,_,_,_,X,_,_,_]
--
rotSwapRight :: [Cell] -> [Cell]
rotSwapRight xs = case xs of a:b:c:d:e:f:g:h:i:xs -> g:d:c:h:e:b:i:f:a:xs

-- | Function for rotating the list left, and also swapping 1 and 3
--
-- Examples
--
-- >>> rotSwapLeft [Blank,Occupied X,Blank,Blank,Blank,Blank,Blank,Blank, Blank]
-- [_,_,_,X,_,_,_,_,_]
--
rotSwapLeft :: [Cell] -> [Cell]
rotSwapLeft xs = case xs of a:b:c:d:e:f:g:h:i:xs -> a:f:i:b:e:h:c:d:g:xs