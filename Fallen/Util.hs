module Fallen.Util
( randomSetElement
) where
  import System.Random
  
  randomSetElement rg set = do
    let (index,rg') = randomR (0,length set - 1) rg :: (Int,StdGen)
    (set !! index, rg')