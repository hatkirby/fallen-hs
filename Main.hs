module Main where
  import Fallen.Point
  import Fallen.Tiles
  import Fallen.Map
  import Fallen.Map.Overworld
  import System.Random
  import UI.HSCurses.Curses
  
  -- main :: ()
  main = do
    rg <- getStdGen
    let (m,rg') = initOverworld rg
    initCurses
    keypad stdScr True
    echo False
    cursSet CursorInvisible
    startColor
    mainLoop (75,75) m rg'
    endWin
  
  mainLoop player m rg = do
    render player m
    input <- getCh
    case input of
      KeyLeft -> moveIfPassable player m rg West
      KeyRight -> moveIfPassable player m rg East
      KeyUp -> moveIfPassable player m rg North
      KeyDown -> moveIfPassable player m rg South
      _ -> putStrLn "endgame"
  
  moveIfPassable player m rg dir = do
    let p' = stepInDirection player dir
    let t' = getTileAtPos m p'
    if t' `elem` passableTiles
      then mainLoop p' m rg
      else mainLoop player m rg
  
  -- render :: Point -> Map -> Window -> ()
  render (px,py) m = do
    (h,w) <- scrSize
    let viewportX = px - quot w 2
    let viewportY = py - quot h 2
    let viewport = [(x,y) | y <- [viewportY..viewportY+h-1], x <- [viewportX..viewportX+w-1]]
    let drawch = drawTile . getTileAtPos m
    let viewport_ = map (\p@(x,y) -> (p, drawch p)) viewport
    mapM_ (\((x,y),ch) -> mvAddCh (y-viewportY) (x-viewportX) ch) viewport_
    mvAddCh (py-viewportY) (px-viewportX) (toEnum $ fromEnum '@')
    refresh