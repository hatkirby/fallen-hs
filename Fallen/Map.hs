module Fallen.Map
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
    mapdata :: [(Point, Tile)],
    background :: Tile
    }
  
  -- emptyMap :: Int -> Int -> Tile -> Map
  emptyMap w h t = Map { dimension=(w,h), mapdata=[], background=t }
  
  -- inBounds :: Map -> Point -> Bool
  inBounds m (x,y) = let (w,h) = dimension m in (x >= 0) && (x < w) && (y >= 0) && (y < h)
  
  -- getTileAtPos :: Map -> Point -> Tile
  getTileAtPos m p = fromMaybe (background m) $ lookup p $ mapdata m
  
  -- findTileInMap :: Map -> Tile -> [Point]
  findTileInMap m t = map fst $ filter ((== t) . snd) $ mapdata m
  
  -- updateMap updates the tiles at the given position in the map
  -- if the passed point already exists in the internal array, it is overwritten with the new point
  -- if the passed tile is the background tile, it removes the tile pair from the internal array
  -- updateMap :: Point -> Tile -> Map -> Map
  updateMap p t (Map d xs bg) = if t == bg
    then Map d (filter ((/= p) . fst) xs) bg
    else case lookup p xs of
      Just _ -> Map d (map (\(p1,t1) -> if p1 == p then (p,t) else (p1,t1)) xs) bg
      Nothing -> Map d ((p,t):xs) bg
  
  -- legalMoves :: Map -> Point -> [Tile] -> [Direction]
  legalMoves m p ts = map fst $ filter legal $ map tileDir directions where
    tileDir d = (d, getTileAtPos m $ stepInDirection p d)
    legal (_,t) = t `elem` ts
   
  -- fillMapRect :: Int -> Int -> Int -> Int -> Tile -> Map -> Map
  fillMapRect x y w h t (Map d xs bg) = if bg == t
	  then Map d (filter outBounds xs) bg 
	  else Map d (filter outBounds xs ++ [((px,py), t) | px <- [x..x+w-1], py <- [y..y+h-1]]) bg where
		  outBounds ((px,py),_) = (px < x) || (px >= (x+w)) || (py < y) || (py >= (y+h))
