module Fallen.Map.Overworld
( initOverworld
) where
  import Fallen.Map
  import Fallen.Tiles
  import System.Random
  import Fallen.Util
  import Fallen.Point
  
  -- initOverworld : generate the overworld
  initOverworld rg = do
    let m = emptyMap 150 150 Forest -- create map
    let m' = fillMapRect 0 0 150 150 Grass m -- fill with grass
    let (tr,rg1) = randomR (1,50) rg :: (Int,StdGen)
    let (lr,rg2) = randomR (1,50) rg1 :: (Int,StdGen)
    let (rr,rg3) = randomR (1,50) rg2 :: (Int,StdGen)
    let (br,rg4) = randomR (1,50) rg3 :: (Int,StdGen)
    let (m'',rg5) = roughen 150 tr lr rr br rg4 m' -- add surrounding forest
    let (clumps,rg6) = randomR (30,60) rg5 :: (Int,StdGen)
    let (m2,rg7) = drawClumps clumps rg6 m''
    (m2,rg7)
  
  -- roughen : create the rough forest edges of the overworld
  roughen 0 _ _ _ _ rg m = (m,rg)
  roughen w tr lr rr br rg m = do
	  let m' = fillMapRect (w-1) 0 1 tr Forest m
	  let m'' = fillMapRect 0 (w-1) lr 1 Forest m'
	  let m2 = fillMapRect (150-rr-1) (w-1) rr 1 Forest m''
	  let m2' = fillMapRect (w-1) (150-br-1) 1 br Forest m2
	  let (str,rg1) = randomR (-2,2) rg :: (Int,StdGen)
	  let (slr,rg2) = randomR (-2,2) rg1 :: (Int,StdGen)
	  let (srr,rg3) = randomR (-2,2) rg2 :: (Int,StdGen)
	  let (sbr,rg4) = randomR (-2,2) rg3 :: (Int,StdGen)
	  roughen (w-1) (bound tr str) (bound lr slr) (bound rr srr) (bound br sbr) rg4 m2' where
      bound r s
        | r + s < 1 = 1
        | r + s > 50 = 50
        | otherwise = r + s
  
  -- drawClumps : add in clumps of rocks, water, and trees
  clumpTypes = [Rock, Water, Forest]
  clumpOver = [Rock, Water, Forest, Grass]
  drawClumps 0 rg m = (m,rg)
  drawClumps num rg m = do
    let (area,rg1) = randomR (1,300) rg :: (Int,StdGen)
    let (clumpType,rg2) = randomSetElement rg1 clumpTypes
    let (startPoint,rg3) = randomPoint 0 0 149 149 rg2
    let (m',rg4) = drawClump area clumpType startPoint m rg3
    drawClumps (num-1) rg4 m'
  drawClump 0 clumpType p m rg = (updateMap p clumpType m,rg)
  drawClump area clumpType p m rg = do
    let m' = updateMap p clumpType m
    let legalDirs = legalMoves m' p clumpOver
    if null legalDirs
      then (m',rg)
      else do
        let (nextDir,rg1) = randomSetElement rg legalDirs
        let nextPoint = stepInDirection p nextDir
        drawClump (area-1) clumpType nextPoint m' rg1