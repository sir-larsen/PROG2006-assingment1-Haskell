module Iofuncs where

import Lib
import System.Random

-- | Function for printing winning message
winMessage :: Symbol -> IO ()
winMessage sym = do
    let line = "GAME OVER " ++ show sym ++ " WON"
    putStrLn line

-- | Function for rendering of game board
renderBoard :: [Cell] -> IO ()
renderBoard board = do
  putStrLn $ "# " ++ renderRow firstRow
  putStrLn $ "# " ++ renderRow secondRow
  putStrLn $ "# " ++ renderRow thirdRow
  where firstRow  = take 3 board
        secondRow = drop 3 . take 6 $ board
        thirdRow  = drop 6 board

-- | Human machine game loop
-- DRY concept gets kinda thrown out the window here and with the subsequent game loops
-- Aware of the fact that there is room for lots of improvement in the code
gameLoopHM2 :: Bool -> Symbol -> [Cell] -> IO ()
gameLoopHM2 play sym board = do
    if play == True then do
        renderBoard board
        cell <- getLine

        let command = words cell

        let index = getBoardIndex2 (command !! 0)
        let check = checkIsFree2 board index
        if check == True then do
            if checkRot command == True then do
                if checkRotRight (command !! 1) == True then do
                    let newBoard = assignCell (command !! 0) sym board
                    let newBoard2 = rotSwapRight newBoard
                    
                    --Check winner
                    let win = checkWin sym newBoard2
                    if win == True then do
                        renderBoard newBoard2
                        winMessage sym
                        return ()
                    else do
                        --Check draw
                        if length (possibleMoves4 newBoard2) == 0 then do
                            renderBoard newBoard2
                            putStrLn "DRAW: GAME OVER"
                            return ()
                        else do
                            gameLoopHM2 False O newBoard2
                else do
                    let newBoard = assignCell (command !! 0) sym board
                    let newBoard2 = rotSwapLeft newBoard
                    --Check winner
                    let win = checkWin sym newBoard2
                    if win == True then do
                        renderBoard newBoard2
                        winMessage sym
                        return ()
                    else do
                        --Check draw
                        if length (possibleMoves4 newBoard2) == 0 then do
                            renderBoard newBoard2
                            putStrLn "DRAW: GAME OVER"
                            return ()
                        else do
                            gameLoopHM2 False O newBoard2
            else do
                let newBoard = assignCell (command !! 0) sym board
                
                let win = checkWin sym newBoard
                if win == True then do
                    renderBoard newBoard
                    winMessage sym
                    return ()
                else do
                    if length (possibleMoves4 newBoard) == 0 then do
                        renderBoard newBoard
                        putStrLn "DRAW: GAME OVER"
                        return ()
                    else do
                        gameLoopHM2 False O newBoard
        else do
            putStrLn "GAME OVER X LOST"
            return ()
    else do
        --CPU uses random movement
        num <- getStdRandom $ randomR (0, 8 :: Int)
        let moves = possibleMoves4 board
        let x = elem num moves

        if x == True then do
            let newBoard = assignCell2 num sym board
            --Check win and draw
            let win = checkWin sym newBoard
            if win == True then do
                renderBoard newBoard
                winMessage sym
                return ()
            else do
                if length (possibleMoves4 newBoard) == 0 then do
                    renderBoard newBoard
                    putStrLn "DRAW: GAME OVER"
                    return ()
                else do
                    gameLoopHM2 True X newBoard
        else do gameLoopHM2 False O board

-- | Human vs human game loop
gameLoopHH :: Bool -> Symbol -> [Cell] -> IO ()
gameLoopHH play sym board = do
    if play == True then do
        renderBoard board
        cell <- getLine

        let command = words cell
    
        let index = getBoardIndex2 (command !! 0)
        let check = checkIsFree2 board index
        if check == True then do
            if checkRot command == True then do
                if checkRotRight (command !! 1) == True then do
                    let newBoard = assignCell (command !! 0) sym board
                    let newBoard2 = rotSwapRight newBoard
                    
                    --Check winner
                    let win = checkWin sym newBoard2       -- Have to check both for win because
                    let win2 = checkWin O newBoard2        -- of bug that can happen with the switching
                    if win == True || win2 == True then do -- of the 1 and 3 positions. Same in the other else branch
                        renderBoard newBoard2
                        winMessage sym
                        return ()
                    else do
                        --Check draw
                        if length (possibleMoves4 newBoard2) == 0 then do
                            renderBoard newBoard2
                            putStrLn "DRAW: GAME OVER"
                            return ()
                        else do
                            gameLoopHH False O newBoard2
                else do
                    let newBoard = assignCell (command !! 0) sym board
                    let newBoard2 = rotSwapLeft newBoard
                    --Check winner
                    let win = checkWin sym newBoard2
                    let win2 = checkWin O newBoard2
                    if win == True || win2 == True then do
                        renderBoard newBoard2
                        winMessage sym
                        return ()
                    else do
                        --Check draw
                        if length (possibleMoves4 newBoard2) == 0 then do
                            renderBoard newBoard2
                            putStrLn "DRAW: GAME OVER"
                            return ()
                        else do
                            gameLoopHH False O newBoard2
            else do
                let newBoard = assignCell (command !! 0) sym board
                
                let win = checkWin sym newBoard
                let win2 = checkWin O newBoard
                if win == True || win2 == True then do
                    renderBoard newBoard
                    winMessage sym
                    return ()
                else do
                    if length (possibleMoves4 newBoard) == 0 then do
                        renderBoard newBoard
                        putStrLn "DRAW: GAME OVER"
                        return ()
                    else do
                        gameLoopHH False O newBoard
        else do
            renderBoard board
            putStrLn "GAME OVER X LOST"
            return ()
    else do
        renderBoard board
        cell <- getLine

        let command = words cell

        let index = getBoardIndex2 (command !! 0)
        let check = checkIsFree2 board index
        if check == True then do
            if checkRot command == True then do
                if checkRotRight (command !! 1) == True then do
                    let newBoard = assignCell (command !! 0) sym board
                    let newBoard2 = rotSwapRight newBoard
                    
                    --Check winner
                    let win = checkWin sym newBoard2
                    let win2 = checkWin X newBoard2
                    if win == True || win2 == True then do
                        renderBoard newBoard2
                        winMessage sym
                        return ()
                    else do
                        --Check draw
                        if length (possibleMoves4 newBoard2) == 0 then do
                            renderBoard newBoard2
                            putStrLn "DRAW: GAME OVER"
                            return ()
                        else do
                            gameLoopHH True X newBoard2
                else do
                    let newBoard = assignCell (command !! 0) sym board
                    let newBoard2 = rotSwapLeft newBoard
                    --Check winner
                    let win = checkWin sym newBoard2
                    let win2 = checkWin X newBoard2
                    if win == True || win2 == True then do
                        renderBoard newBoard2
                        winMessage sym
                        return ()
                    else do
                        --Check draw
                        if length (possibleMoves4 newBoard2) == 0 then do
                            renderBoard newBoard2
                            putStrLn "DRAW: GAME OVER"
                            return ()
                        else do
                            gameLoopHH True X newBoard2
            else do
                let newBoard = assignCell (command !! 0) sym board
                
                let win = checkWin sym newBoard
                let win2 = checkWin X newBoard
                if win == True || win2 == True then do
                    renderBoard newBoard
                    winMessage sym
                    return ()
                else do
                    if length (possibleMoves4 newBoard) == 0 then do
                        renderBoard newBoard
                        putStrLn "DRAW: GAME OVER"
                        return ()
                    else do
                        gameLoopHH True X newBoard
        else do
            renderBoard board
            putStrLn "GAME OVER O LOST"
            return ()

-- | Machine vs machine game loop
gameLoopMM :: Bool -> Symbol -> [Cell] -> IO ()
gameLoopMM play sym board = do
    if play == True then do
        num <- getStdRandom $ randomR (0, 8 :: Int) -- Get random move
        let moves = possibleMoves4 board
        let x = elem num moves

        if x == True then do
            let newBoard = assignCell2 num sym board
            renderBoard board
            putStrLn ""
            --Check win and draw
            let win = checkWin sym newBoard
            if win == True then do
                renderBoard newBoard
                winMessage sym
                return ()
            else do
                if length (possibleMoves4 newBoard) == 0 then do
                    renderBoard newBoard
                    putStrLn "DRAW: GAME OVER"
                    return ()
                else do
                    gameLoopMM False O newBoard
        else do
            gameLoopMM True X board
    else do
        num <- getStdRandom $ randomR (0, 8 :: Int)
        let moves = possibleMoves4 board
        let x = elem num moves

        if x == True then do
            let newBoard = assignCell2 num sym board
            renderBoard board
            putStrLn ""
            --Check win and draw
            let win = checkWin sym newBoard
            if win == True then do
                renderBoard newBoard
                winMessage sym
                return ()
            else do
                if length (possibleMoves4 newBoard) == 0 then do
                    renderBoard newBoard
                    putStrLn "DRAW: GAME OVER"
                    return ()
                else do
                    gameLoopMM True X newBoard
        else do
            gameLoopMM False O board

