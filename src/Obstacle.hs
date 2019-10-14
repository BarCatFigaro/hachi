module Obstacle where

    import Graphics.UI.Fungen
    import System.Random

    import Types
    import Graphics

    initObsSize :: (Double, Double)
    initObsSize = (30, 30)

    -- TODO randomize x values of objects --
    obsPosX :: Double
    obsPosX = fromIntegral $ fst windowSize

    mObsPosY :: IO Double
    mObsPosY = do
        randY <- randomRIO (1, 3 :: Int)
        return $ maxHeight / fromIntegral randY

    -- TODO append n to the object key --    
    createObstacle :: Int -> IO Obstacle
    createObstacle n = do
        obsPosY <- mObsPosY
        return $ object "ball" (initPicture initObsSize (n + 4)) False (obsPosX, obsPosY) (-5, 0) ()

    createObstacles :: Int -> IO [Obstacle]
    createObstacles 0 = return []
    createObstacles n = do
        obstacle <- createObstacle (n `mod` 4)
        obstacles <- createObstacles $ n - 1
        return $ obstacle : obstacles
