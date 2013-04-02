module Main where
  import Fallen.Point
  import Fallen.Tiles
  import Fallen.Map
  import Fallen.Map.Overworld
  import System.Random
  import UI.HSCurses.Curses
  
  -- main :: ()
  main = do
    m <- initOverworld
    initCurses
    keypad stdScr True
    echo False
    cursSet CursorInvisible
    startColor
    mainLoop (75,75) m
    endWin
  
  mainLoop player m = do
    render player m
    input <- getCh
    case input of
      KeyLeft -> moveIfPassable player m West
      KeyRight -> moveIfPassable player m East
      KeyUp -> moveIfPassable player m North
      KeyDown -> moveIfPassable player m South
      _ -> putStrLn "endgame"
  
  moveIfPassable player m dir = do
    let p' = stepInDirection player dir
    let t' = getTileAtPos m p'
    if t' `elem` passableTiles
      then mainLoop p' m
      else mainLoop player m
  
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