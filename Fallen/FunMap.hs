-- Alternate implementation of Map with functions

module Fallen.FunMap
( Map,
  emptyMap,
  dimension,
  inBounds,
  getTileAtPos,
  findTileInMap,
  updateMap,
  legalMoves,
  fillMapRect
) where
  import Fallen.Tiles
  import Fallen.Point
  import Data.List
  import Fallen.Util
  import Data.Maybe
  
  data Map = Map {
    dimension :: (Int, Int),
    mapdata :: Point -> Tile,
    background :: Tile
    }
  
  -- emptyMap :: Int -> Int -> Tile -> Map
  emptyMap w h t = Map { dimension=(w,h), mapdata= const t, background=t }
  
  -- inBounds :: Map -> Point -> Bool
  inBounds m (x,y) = let (w,h) = dimension m in (x >= 0) && (x < w) && (y >= 0) && (y < h)
  
  -- getTileAtPos :: Map -> Point -> Tile
  getTileAtPos m p = if inBounds m p
    then mapdata m p
    else background m
  
  -- findTileInMap :: Map -> Tile -> [Point]
  -- REALLY inefficient
  findTileInMap m t = filter (==t) $ map (mapdata m) rawPoints where
    (w,h) = dimension m
    rawPoints = [(x,y) | x <- [0..w-1], y <- [0..h-1]]
  
  -- updateMap :: Point -> Tile -> Map -> Map
  updateMap p t (Map d xs bg) = Map d (redirect p t xs) bg where
    redirect p t xs p2 = if p == p2 then t else xs p2
  
  -- legalMoves :: Map -> Point -> [Tile] -> [Direction]
  legalMoves m p ts = map fst $ filter legal $ map tileDir directions where
    tileDir d = (d, getTileAtPos m $ stepInDirection p d)
    legal (_,t) = t `elem` ts
   
  -- fillMapRect :: Int -> Int -> Int -> Int -> Tile -> Map -> Map
  fillMapRect x y w h t (Map d xs bg) = Map d redirectInBounds bg where
    redirectInBounds p = if inBounds p then t else xs p
    inBounds (px,py) = (px >= x) && (px < (x+w)) && (py >= y) && (py < (y+h))
  