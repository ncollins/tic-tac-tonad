-- Tic Tac Toe in Haskell

import qualified Data.Map as M
import qualified Data.Char as C

type Square = (Int, Int)
data Piece = X | O | NoPiece deriving (Eq, Show)
data Board = Board Int (M.Map Square Piece) -- the Int is the size
data State = Tie | Victory | Continue | Abort | Repeat deriving (Eq, Show)
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
    let findPiece = flip (M.findWithDefault NoPiece) m
        lineCoords = concatMap (\f -> f board) [boardRows, boardColumns, boardDiagonals]
        lines = map (map findPiece) lineCoords
    in
      if any (\line -> (all (== X) line) || (all (== O) line)) lines
      then Victory
      else if any (== NoPiece) (concat lines)
           then Continue
           else Tie

main = do
    putStrLn "Welcome to Tic-Tac-Monad!"
    let initialBoard = Board 3 (M.fromList [])
    putStr $ showBoard initialBoard
    gameover <- mainLoop initialBoard X
    return ()


mainLoop board piece = do
    putStrLn $ "enter your move " ++ (show piece) ++ ":"
    input <- fmap parseInput getLine
    putStrLn (show input)
    let (state, newPiece, newBoard) = (evalInput input piece board)
    case state of
           Repeat -> do putStrLn "invalid input, try again..."
                        mainLoop newBoard newPiece
           Abort  -> do putStrLn "Game ended."
                        return ()
           Victory -> do putStrLn $ (show piece) ++ " Wins!"
                         putStr $ showBoard newBoard
                         return ()
           Tie     -> do putStrLn $ "Game over - no more moves left"
                         putStr $ showBoard newBoard
                         return ()
           Continue -> do putStrLn "test"
                          putStr $ showBoard newBoard
                          mainLoop newBoard newPiece


evalInput :: Command -> Piece -> Board -> (State, Piece, Board)
evalInput Exit piece board = (Abort, piece, board)
evalInput InvalidInput piece board = (Repeat, piece, board)
evalInput (Move square) piece board = 
    let maybeBoard = move board square piece
    in
       case maybeBoard of
           Nothing -> (Repeat, piece, board)
           Just newBoard -> (gameState newBoard, if piece == X then O else X, newBoard)

parseInput :: String -> Command
parseInput "end" = Exit
parseInput (c:r:[]) = if (C.isDigit) r && (C.isAlpha c)
                         then let col = (-96) + (C.ord (C.toLower c))
                                  row = C.digitToInt r
                              in Move (row, col)
                         else InvalidInput
parseInput _ = InvalidInput
