module Fallen.Util
( randomSetElement
) where
  import System.Random
  
  randomSetElement set = do
    index <- randomRIO (0, length set - 1)
    return $ set !! index