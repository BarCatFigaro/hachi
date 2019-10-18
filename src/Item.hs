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
    jumpPowerInfo = PowerUpInfo False JumpPower 5

    noClipPowerInfo :: PowerUpInfo
    noClipPowerInfo = PowerUpInfo False NoClipPower 10000

    itemPosX :: Double
    itemPosX = fromIntegral $ fst windowSize

    mItemPosY :: IO Double
    mItemPosY = do
        randY <- randomRIO (1, 3 :: Int)
        return $ maxHeight / fromIntegral randY

    createItem :: IO Item
    createItem = do
        itemPosY <- mItemPosY
        k <- randomRIO (1, 2 :: Int)
        n <- randomIO
        return (if isPower k then createPowerUp itemPosY n else createObstacle itemPosY n)
        where
            isPower k = k == 2

    createObstacle :: Double -> Int -> Item
    createObstacle itemPosY n = object itemName (initPicture (getPicSize (modN n) obstacles) ((modN n) + 4)) False (itemPosX, itemPosY) (-45, 0) ()
        where
            modN n = n `mod` (length obstacles)

    createPowerUp :: Double -> Int -> Item
    createPowerUp itemPosY n = object powerUpName (initPicture (getPicSize (modN n) powerUps) ((modN n) + 8)) False (itemPosX, itemPosY) (-45, 0) ()
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

    reinitItem :: Item -> GameAction ()
    reinitItem item = do
        (pX, pY) <- getObjectPosition item
        newPosY <- liftIOtoIOGame mItemPosY
        if pX < 0
            then do
                destroyObject item
                item <- liftIOtoIOGame createItem
                addObjectsToGroup [item] "itemGroup"
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