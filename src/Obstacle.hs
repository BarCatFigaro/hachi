module Obstacle where

    import Graphics.UI.Fungen

    import Types
    import Graphics

    obstacle :: Obstacle
    obstacle = object "ball" (Tex (192, 192) 4) False (fromIntegral (fst windowSize), fromIntegral (snd windowSize `div` 8)) (-5, 0) ()
