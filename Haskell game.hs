type Position = (Int, Int)
data Move = M Position deriving (Show,Eq)
data Color = W | B deriving (Eq, Show)
data Peg = Peg Position Color deriving (Eq, Show)
type Board = [Peg]
data State = S Move Board deriving (Eq, Show)


boardpositions :: [Position]
boardpositions =
  [ (-3, -1), (-3, 0), (-3, 1),
    (-2, -1), (-2, 0), (-2, 1),
    (-1, -3), (-1, -2), (-1, -1), (-1, 0), (-1, 1), (-1, 2), (-1, 3),
    ( 0, -3), ( 0, -2), ( 0, -1), ( 0, 0), ( 0, 1), ( 0, 2), ( 0, 3),
    ( 1, -3), ( 1, -2), ( 1, -1), ( 1, 0), ( 1, 1), ( 1, 2), ( 1, 3),
    ( 2, -1), ( 2, 0), ( 2, 1),
    ( 3, -1), ( 3, 0), ( 3, 1)
  ]
createBoard :: Position -> Board
createBoard pos
  | notElem pos boardpositions = error "Program error: The position is not valid."
  | otherwise = [if p == pos then Peg p W else Peg p B | p <- boardpositions]


isValidMove :: Move -> Board -> Bool
isValidMove _ [] = False
isValidMove (M (x, y)) ((Peg b g):t)
    | b == (x, y) && isWhite g = False  -- The peg itself is white, cannot be flipped
    | b == (x, y) && not (isWhite g) = hasAdjacentWhite (x, y) t  -- The peg is black, check for adjacent white
    | otherwise = isValidMove (M (x, y)) t
isAdjacent :: Position -> Position -> Bool
isAdjacent (x1,y1) (x2,y2) = (x1 == x2 && abs (y1 - y2) == 1) || (y1 == y2 && abs (x1 - x2) == 1)

hasAdjacentWhite :: Position -> Board -> Bool
hasAdjacentWhite _ [] = False
hasAdjacentWhite pos ((Peg b g):t)
    | isAdjacent pos b && isWhite g = True
    | otherwise = hasAdjacentWhite pos t

isWhite :: Color -> Bool
isWhite W = True
isWhite _ = False
-- Function to generate all valid moves for a given board
validMoves :: Board -> [Move]
validMoves board = [M pos | pos <- boardpositions, isValidMove (M pos) board]

-- Function to apply a move to a board
applyMove :: Move -> Board -> Board
applyMove (M pos) board = map updatePeg board
    where
      updatePeg (Peg p c) = if p == pos then Peg p W else Peg p c

helper moves board = map (\move -> S move (applyMove move board)) moves 

showPossibleNextStates:: Board -> [State]
showPossibleNextStates board = if (isGoal board) then error "No Possible States Exist." else helper (validMoves board) board 


isGoal [] = True
isGoal ((Peg pos color):t) = if isWhite color then isGoal t else False 

