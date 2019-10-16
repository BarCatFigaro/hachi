module Obstacle where

    import Graphics.UI.Fungen
    import System.Random

    import Types
    import Graphics

    obstacleName :: String
    obstacleName = "ball"

    powerUpName :: String
    powerUpName = "power"

    initObsSize :: (Double, Double)
    initObsSize = (30, 30)

    obsPosX :: Double
    obsPosX = fromIntegral $ fst windowSize

    mObsPosY :: IO Double
    mObsPosY = do
        randY <- randomRIO (1, 3 :: Int)
        return $ maxHeight / fromIntegral randY

    createObstacle :: Int -> IO Obstacle
    createObstacle n = do
        obsPosY <- mObsPosY
        k <- randomRIO (1, 2 :: Int)
        return $ object (name k) (initPicture initObsSize (idx k n)) False (obsPosX, obsPosY) (-45, 0) ()
        where
            isPower k = k == 2
            name k = if isPower k then powerUpName else obstacleName
            idx k n = if isPower k then n + 8 else n + 4

    createObstacles :: Int -> IO [Obstacle]
    createObstacles 0 = return []
    createObstacles n = do
        obstacle <- createObstacle $ n `mod` 4
        obstacles <- createObstacles $ n - 1
        return $ obstacle : obstacles

    resetPosX :: Obstacle -> GameAction ()
    resetPosX obs = do
        (pX, pY) <- getObjectPosition obs
        newPosY <- liftIOtoIOGame mObsPosY
        if pX < 0
            then setObjectPosition (obsPosX, newPosY) obs
            else return ()

    resetObsPos :: [Obstacle] -> GameAction ()    
    resetObsPos [] = return ()
    resetObsPos (x:xs) = do
        resetPosX x
        resetObsPos xs

    obstacleCycle :: GameAction ()
    obstacleCycle = do
        obstacles <- getObjectsFromGroup "obstacleGroup"
        resetObsPos obstacles