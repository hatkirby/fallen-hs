module Fallen.Tiles
( Tile(Grass, Earth, Passage, Floor, Forest, Rock, Water),
  drawTile,
  passableTiles
) where
  import UI.HSCurses.CursesHelper
  
  data Tile = Grass | Earth | Passage | Floor | Forest | Rock | Water deriving (Show)
  
  instance Eq Tile where
    Grass == Grass = True
    Earth == Earth = True
    Passage == Passage = True
    Floor == Floor = True
    Forest == Forest = True
    Rock == Rock = True
    Water == Water = True
    _ == _ = False
  
  drawCharWithColor ch color = toEnum $ fromEnum ch
    
  drawTile Grass = drawCharWithColor '.' GreenF
  drawTile Earth = drawCharWithColor ' ' BlackF
  drawTile Passage = drawCharWithColor '.' GreyF
  drawTile Floor = drawCharWithColor '.' GreyF
  drawTile Forest = drawCharWithColor '~' GreenF
  drawTile Rock = drawCharWithColor '*' GreyF
  drawTile Water = drawCharWithColor '~' BlueF
  
  passableTiles = [Grass, Passage, Floor]