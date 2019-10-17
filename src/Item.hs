module Item where

    import Graphics.UI.Fungen
    import System.Random

    import Types
    import Graphics

    itemName :: String
    itemName = "ball"

    powerUpName :: String
    powerUpName = "power"

    jumpPowerInfo :: PowerUpInfo
    jumpPowerInfo = PowerUpInfo False (JumpPower 0) 5

    noClipPowerInfo :: PowerUpInfo
    noClipPowerInfo = PowerUpInfo False (NoClipPower 0) 5

    initObsSize :: (Double, Double)
    initObsSize = (30, 30)

    obsPosX :: Double
    obsPosX = fromIntegral $ fst windowSize

    mObsPosY :: IO Double
    mObsPosY = do
        randY <- randomRIO (1, 3 :: Int)
        return $ maxHeight / fromIntegral randY

    -- TODO fix monad typing: we are running into an issue relating IO and IOGame --
    createItem :: IO Item
    createItem = do
        obsPosY <- mObsPosY
        k <- randomRIO (1, 2 :: Int)
        n <- randomIO
        return (if isPower k then createPowerUp obsPosY n else createObstacle obsPosY n)
        where
            isPower k = k == 2

    createObstacle :: Double -> Int -> Item
    createObstacle obsPosY n = object itemName (initPicture initObsSize ((modN n) + 4)) False (obsPosX, obsPosY) (-45, 0) ()
        where
            modN n = n `mod` (length obstacles)

    createPowerUp :: Double -> Int -> Item
    createPowerUp obsPosY n = object powerUpName (initPicture initObsSize ((modN n) + 8)) False (obsPosX, obsPosY) (-45, 0) ()
        where
            modN n = n `mod` (length powerUps)
        
    setPowerUp :: Int -> GameAction ()
    setPowerUp n = do
        (GameAttribute score isJump dogState _) <- getGameAttribute
        setGameAttribute (GameAttribute score isJump dogState (powerUpInfo n))
        where
            powerUpInfo n
                | n > 1 = jumpPowerInfo
                | otherwise = noClipPowerInfo


    createItems :: Int -> IO [Item]
    createItems 0 = return []
    createItems n = do
        item <- createItem
        items <- createItems $ n - 1
        return $ item : items

    reinitObsHelper :: Item -> GameAction ()
    reinitObsHelper obs = do
        (pX, pY) <- getObjectPosition obs
        newPosY <- liftIOtoIOGame mObsPosY
        if pX < 0
            then do
                destroyObject obs
                items <- liftIOtoIOGame (createItems 1)
                addObjectsToGroup items "itemGroup"
            else return ()

    reinitObs :: [Item] -> GameAction ()    
    reinitObs [] = return ()
    reinitObs (x:xs) = do
        reinitObsHelper x
        reinitObs xs

    itemCycle :: GameAction ()
    itemCycle = do
        items <- getObjectsFromGroup "itemGroup"
        reinitObs items