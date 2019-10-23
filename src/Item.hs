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
    jumpPowerInfo = PowerUpInfo False JumpPower 140

    noClipPowerInfo :: PowerUpInfo
    noClipPowerInfo = PowerUpInfo False NoClipPower 140

    initItemSpeed :: Double
    initItemSpeed = -45

    speedStepper :: Double
    speedStepper = 25

    itemPosX :: Double
    itemPosX = fromIntegral $ fst windowSize

    mItemPosY :: IO Double
    mItemPosY = do
        randY <- randomRIO (1, 3 :: Int)
        return $ maxHeight / fromIntegral randY

    createItem :: GameAction Item
    createItem = do
        k <- liftIOtoIOGame $ randomRIO (1, 3 :: Int)
        (GameAttribute score _ _ _) <- getGameAttribute
        if isPower k then (createPowerUp $ fromIntegral score) else (liftIOtoIOGame $ createObstacle $ fromIntegral score)
        where
            isPower k = k == 2

    createObstacle :: Double -> IO Item
    createObstacle score = do
        n <- randomIO
        itemPosY <- mItemPosY
        return $ object itemName (obstaclePicture n) False (itemPosX, itemPosY) (speed score) ()
        where
            obstaclePicture n = (initPicture (getPicSize (modN n) obstacles) ((modN n) + 4))
            speed score = (initItemSpeed - score / speedStepper, 0)
            modN n = n `mod` (length obstacles)

    createPowerUp :: Double -> GameAction Item
    createPowerUp score = do
        n <- liftIOtoIOGame randomIO
        itemPosY <- liftIOtoIOGame mItemPosY
        setPowerUp n
        liftIOtoIOGame $ powerUpIO n itemPosY score
        where
            powerUpPicture n = (initPicture (getPicSize (modN n) powerUps) ((modN n) + 8))
            speed score = (initItemSpeed - score / speedStepper, 0)
            powerUpIO n itemPosY score = return $ object powerUpName (powerUpPicture n)  False (itemPosX, itemPosY) (speed score) ()
            modN n = n `mod` (length powerUps)
        
    setPowerUp :: Int -> GameAction ()
    setPowerUp n = do
        (GameAttribute score isJump dogState _) <- getGameAttribute
        setGameAttribute (GameAttribute score isJump dogState (powerUpInfo $ modN n))
        where
            modN n = n `mod` (length powerUps)
            powerUpInfo n
                | n > 1 = jumpPowerInfo
                | otherwise = noClipPowerInfo

    reinitItem :: Item -> GameAction ()
    reinitItem item = do
        (pX, pY) <- getObjectPosition item
        newPosY <- liftIOtoIOGame mItemPosY
        if pX < 0
            then do
                destroyObject item
                newItem <- createItem
                addObjectsToGroup [newItem] "itemGroup"
            else return ()

    reinitItems :: [Item] -> GameAction ()    
    reinitItems [] = return ()
    reinitItems (x:xs) = do
        reinitItem x
        reinitItems xs

    itemCycle :: GameAction ()
    itemCycle = do
        items <- getObjectsFromGroup "itemGroup"
        reinitItems items