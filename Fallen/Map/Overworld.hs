module Fallen.Map.Overworld
( initOverworld
) where
  import Fallen.Map
  import Fallen.Tiles
  import System.Random
  import Fallen.Util
  import Fallen.Point
  
  -- initOverworld : generate the overworld
  initOverworld = do
    let m = emptyMap 150 150 Forest -- create map
    let m1 = fillMapRect 0 0 150 150 Grass m -- fill with grass
    tr <- randomRIO (1,50)
    lr <- randomRIO (1,50)
    rr <- randomRIO (1,50)
    br <- randomRIO (1,50)
    m2 <- roughen 150 tr lr rr br m1 -- add surrounding forest
    clumps <- randomRIO (30,60)
    m3 <- drawClumps clumps m2
    return m3
  
  -- roughen : create the rough forest edges of the overworld
  roughen 0 _ _ _ _ m = return m
  roughen w tr lr rr br m = do
    let m1 = fillMapRect (w-1) 0 1 tr Forest m
    let m2 = fillMapRect 0 (w-1) lr 1 Forest m1
    let m3 = fillMapRect (150-rr-1) (w-1) rr 1 Forest m2
    let m4 = fillMapRect (w-1) (150-br-1) 1 br Forest m3
    str <- randomRIO (-2,2)
    slr <- randomRIO (-2,2)
    srr <- randomRIO (-2,2)
    sbr <- randomRIO (-2,2)
    roughen (w-1) (bound tr str) (bound lr slr) (bound rr srr) (bound br sbr) m4 where
      bound r s
        | r + s < 1 = 1
        | r + s > 50 = 50
        | otherwise = r + s
  
  -- drawClumps : add in clumps of rocks, water, and trees
  clumpTypes = [Rock, Water, Forest]
  clumpOver = [Rock, Water, Forest, Grass]
  drawClumps :: Int -> Map -> IO Map
  drawClumps 0 m = return m
  drawClumps num m = do
    area <- randomRIO (1,300)
    clumpType <- randomSetElement clumpTypes
    startPoint <- randomPoint 0 0 149 149
    m' <- drawClump area clumpType startPoint m
    drawClumps (num-1) m'
  drawClump :: Int -> Tile -> Point -> Map -> IO Map
  drawClump 0 clumpType p m = return $ updateMap p clumpType m
  drawClump area clumpType p m = do
    let m' = updateMap p clumpType m
    let legalDirs = legalMoves m' p clumpOver
    if null legalDirs
      then return m'
      else do
        nextDir <- randomSetElement legalDirs
        let nextPoint = stepInDirection p nextDir
        drawClump (area-1) clumpType nextPoint m'