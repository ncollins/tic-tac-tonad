-- Tic Tac Toe in Haskell

import qualified Data.Map as M
import qualified Data.Char as C

type Square = (Int, Int)
data Piece = X | O | NoPiece deriving (Eq, Show)
data Board = Board Int (M.Map Square Piece) -- the Int is the size
data GameState = Tie | Victory | Continue deriving (Eq, Show)
data Command = Exit | Move Square | InvalidInput deriving (Eq, Show)

showBoard :: Board -> String
showBoard (Board size m) = let lookup sq = M.findWithDefault " " sq (fmap show m)
                           in
                              "--------\n" ++
                              "  " ++ (unwords ["A", "B", "C"]) ++ "\n" ++
                              "1 " ++ (unwords $ map lookup [(1,1), (1,2), (1,3)]) ++ "\n" ++
                              "2 " ++ (unwords $ map lookup [(2,1), (2,2), (2,3)]) ++ "\n" ++
                              "3 " ++ (unwords $ map lookup [(3,1), (3,2), (3,3)]) ++ "\n" ++
                              "--------\n"

squareTaken (Board size m) square = M.member square m

move (Board size m) s p = if squareTaken (Board size m) s
                               then Nothing
                               else Just $ Board size (M.insert s p m)


-- i.e. it only checks if the most recent move has ended the game
victory (Board size m) (row, column) piece =
    let row_coords = map (\y -> (row, y)) [1..size]
        col_coords = map (\x -> (x, column)) [1..size]
        squareEq p s = (M.findWithDefault NoPiece s m) == p
    in all (squareEq piece) row_coords || all (squareEq piece) col_coords


boardRows (Board size m) =
    let range = [1..size]
    in map (\r -> map (\c -> (r,c)) range) range


boardColumns (Board size m) =
    let range = [1..size]
    in map (\c -> map (\r -> (r,c)) range) range

boardDiagonals (Board size m) =
    let range = [1..size]
    in [zip range range, zip range (reverse range)]

gameState board@(Board size m) =
    let rows = map (\l -> map (\s -> M.findWithDefault NoPiece s m) l) (boardRows board)
        columns = map (\l -> map (\s -> M.findWithDefault NoPiece s m) l) (boardColumns board)
        diagonals = map (\l -> map (\s -> M.findWithDefault NoPiece s m) l) (boardDiagonals board)
        lines = rows ++ columns ++ diagonals
    in
      if any (\line -> (all (\x -> x == X) line) || (all (\x -> x == O) line)) lines
      then Victory
      else if all (\x -> x /= NoPiece) (concat rows)
           then Tie
           else Continue
          
main = do
    putStrLn "Welcome to Tic-Tac-Monad!"
    let initialBoard = Board 3 (M.fromList [])
    putStr $ showBoard initialBoard
    gameover <- mainLoop initialBoard X
    return ()


mainLoop board piece = do
    putStrLn $ "enter your move " ++ (show piece) ++ ":"
    input <- fmap parseInput getLine
    case input of
        Exit -> return ()
        InvalidInput -> do putStrLn "invalid input, try again..."
                           mainLoop board piece
        Move square -> do let maybeBoard = move board square piece
                           in 
                              case maybeBoard of
                                  Nothing -> do putStrLn "invalid input, try again..."
                                                mainLoop board piece
                                  Just newBoard -> case gameState newBoard of
                                      Victory -> do putStrLn $ (show piece) ++ " Wins!"
                                                    putStr $ showBoard newBoard
                                                    return ()
                                      Tie     -> do putStrLn $ "Game over - no more moves left"
                                                    putStr $ showBoard newBoard
                                                    return ()
                                      Continue -> let nextPiece = if piece == X then O else X
                                                  in do putStr $ showBoard newBoard
                                                        mainLoop newBoard nextPiece


parseInput :: String -> Command
parseInput "end" = Exit
parseInput (r:s:c:[]) = if (C.isDigit) r && (C.isDigit c) && (' ' == s)
                         then Move (C.digitToInt r, C.digitToInt c)
                         else InvalidInput
parseInput _ = InvalidInput
