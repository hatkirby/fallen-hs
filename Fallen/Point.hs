module Fallen.Point
( Point,
  Direction(North, South, East, West),
  directions,
  distance,
  stepInDirection,
  opposite,
  randomPoint,
  dirToPoint
) where
  import System.Random
  
  type Point = (Int, Int)
  data Direction = North | South | East | West deriving (Show)
  
  -- directions :: [Direction]
  directions = [North, South, East, West]
  
  -- distance :: Point -> Point -> Int
  distance (x1, y1) (x2, y2) = ceiling . sqrt . fromIntegral $ (x1-x2)^2 + (y1-y2)^2
  
  -- stepInDirection :: Point -> Direction -> Point
  stepInDirection (x, y) dir = case dir of
    North -> (x, y-1)
    South -> (x, y+1)
    East -> (x+1, y)
    West -> (x-1, y)
  
  -- opposite :: Direction -> Direction
  opposite dir = case dir of
    North -> South
    South -> North
    East -> West
    West -> East
  
  -- randomPoint :: Int -> Int -> Int -> Int -> IO Point
  randomPoint minX minY maxX maxY rg = do
    let (x,rg1) = randomR (minX, maxX) rg :: (Int,StdGen)
    let (y,rg2) = randomR (minY, maxY) rg1  :: (Int,StdGen)
    ((x,y),rg2)
  
  -- dirToPoint :: Point -> Point -> [Direction]
  dirToPoint p1 p2 = horizDirToPoint p1 p2 ++ vertDirToPoint p1 p2 where
    horizDirToPoint (x1,y1) (x2,y2) = case compare x1 x2 of
      LT -> [East]
      EQ -> []
      GT -> [West]
    vertDirToPoint (x1,y1) (x2,y2) = case compare y1 y2 of
      LT -> [North]
      EQ -> []
      GT -> [South]
  
  instance Eq Direction where
    North == North = True
    South == South = True
    East == East = True
    West == West = True
    _ == _ = False
