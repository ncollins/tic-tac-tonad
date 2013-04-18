-- Tic Tac Toe in Haskell

import qualified Data.Map as M
import qualified Data.Char as C

type Square = (Int, Int)
data Piece = X | O | NoPiece deriving (Eq)

instance Show Piece where
  show X = "X"
  show O = "O"
  show _ = " "

instance Show Board where
  show (Board size m) = unlines $
    border : header : (map showRow $ boardRows size) ++ [border]
    where
      showSq sq = show $ M.findWithDefault NoPiece sq m
      showRow r = unwords $ (show . fst $ head r) : map showSq r
      border = "--------"
      header = "  A B C"

data Board = Board Int (M.Map Square Piece) -- the Int is the size
data State = Tie | Victory | Continue | Abort | Repeat deriving (Eq, Show)
data Command = Exit | Move Square | InvalidInput deriving (Eq, Show)


move (Board size m) s p
  | M.member s m = Nothing
  | otherwise = Just $ Board size (M.insert s p m)

boardRows size =
    let range = [1..size]
    in map (flip zip range . repeat) range


boardColumns size =
    let range = [1..size]
    in map (zip range . repeat) range

boardDiagonals size =
    let range = [1..size]
    in [zip range range, zip range (reverse range)]


gameState (Board size m)
  | any won ls                   = Victory
  | any (== NoPiece) (concat ls) = Continue
  | otherwise                    = Tie
  where
    ls = map (map findPiece) lineCoords
    findPiece = flip (M.findWithDefault NoPiece) m
    lineCoords = concatMap ($size) [boardRows, boardColumns, boardDiagonals]
    won l = all (== X) l || all (== O) l


main = do
    putStrLn "Welcome to Tic-Tac-Monad!"
    let initialBoard = Board 3 (M.fromList [])
    putStr $ show initialBoard
    mainLoop initialBoard X


mainLoop board piece = do
    putStrLn $ "enter your move " ++ show piece ++ ":"
    input <- fmap parseInput getLine
    putStrLn $ show input
    let (state, newPiece, newBoard) = evalInput input piece board
    case state of
           Repeat   -> do putStrLn "invalid input, try again..."
                          mainLoop newBoard newPiece
           Victory  -> do putStrLn $ (show piece) ++ " Wins!"
                          putStr $ show newBoard
           Tie      -> do putStrLn $ "Game over - no more moves left"
                          putStr $ show newBoard
           Continue -> do putStrLn "test"
                          putStr $ show newBoard
                          mainLoop newBoard newPiece
           Abort    -> putStrLn "Game ended."


evalInput :: Command -> Piece -> Board -> (State, Piece, Board)
evalInput Exit piece board = (Abort, piece, board)
evalInput InvalidInput piece board = (Repeat, piece, board)
evalInput (Move square) piece board =
    case move board square piece of
       Nothing       -> (Repeat, piece, board)
       Just newBoard -> (gameState newBoard, if piece == X then O else X, newBoard)

parseInput :: String -> Command
parseInput "end" = Exit
parseInput (c:r:[])
  | C.isDigit r && C.isAlpha c = 
    let col = C.ord (C.toLower c) - 96
        row = C.digitToInt r
    in Move (row, col)
  | otherwise = InvalidInput
parseInput _ = InvalidInput
