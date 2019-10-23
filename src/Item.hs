module Item where

    import Graphics.UI.Fungen
    import System.Random

    import Types
    import Graphics

    -- obstacleName is the name of the object for an obstacle
    obstacleName :: String
    obstacleName = "ball"

    -- powerUpName is the name of the object for a powerUp
    powerUpName :: String
    powerUpName = "power"

    -- jumpPowerInfo represents the jumping powerup
    jumpPowerInfo :: PowerUpInfo
    jumpPowerInfo = PowerUpInfo False JumpPower 140

    -- noClipPowerInfo represents the no clip powerup
    noClipPowerInfo :: PowerUpInfo
    noClipPowerInfo = PowerUpInfo False NoClipPower 140

    -- initItemSpeed is the initial speed for all items (obstacles and powerups)
    initItemSpeed :: Double
    initItemSpeed = -45

    -- speedStepper is the rate the speed of the game (items) is increased
    ----- lower meaning faster
    speedStepper :: Double
    speedStepper = 25

    -- itemPosX is the starting X position of all items; the right edge of the screen
    itemPosX :: Double
    itemPosX = fromIntegral $ fst windowSize

    -- mItemPosY is the Y position of items, randomized by 3 rows based on a max height
    mItemPosY :: IO Double
    mItemPosY = do
        randY <- randomRIO (1, 3 :: Int)
        return $ maxHeight / fromIntegral randY

    -- createItem randomly creates an obstacle or powerup
    ----- currently 1/3 chance to generate a powerup
    createItem :: GameAction Item
    createItem = do
        k <- liftIOtoIOGame $ randomRIO (1, 3 :: Int)
        (GameAttribute score _ _ _) <- getGameAttribute
        if isPower k then (createPowerUp $ fromIntegral score) else (liftIOtoIOGame $ createObstacle $ fromIntegral score)
        where
            isPower k = k == 2

    -- createObstacle creates a new obstacle with a randomized Y position
    -- score the current score of the game
    createObstacle :: Double -> IO Item
    createObstacle score = do
        n <- randomIO
        itemPosY <- mItemPosY
        return $ object obstacleName (obstaclePicture n) False (itemPosX, itemPosY) (speed score) ()
        where
            obstaclePicture n = (initPicture (getPicSize (modN n) obstacles) ((modN n) + (length dogs)))
            speed score = (initItemSpeed - score / speedStepper, 0)
            modN n = n `mod` (length obstacles)

    -- createPowerUp creates a new powerup with a randomized Y position
    -- score the current score of the game
    createPowerUp :: Double -> GameAction Item
    createPowerUp score = do
        n <- liftIOtoIOGame randomIO
        itemPosY <- liftIOtoIOGame mItemPosY
        setPowerUp n
        liftIOtoIOGame $ powerUpIO n itemPosY score
        where
            powerUpPicture n = (initPicture (getPicSize (modN n) powerUps) ((modN n) + ((length dogs) + (length obstacles))))
            speed score = (initItemSpeed - score / speedStepper, 0)
            powerUpIO n itemPosY score = return $ object powerUpName (powerUpPicture n)  False (itemPosX, itemPosY) (speed score) ()
            modN n = n `mod` (length powerUps)
        
    -- setPowerUp decides whether the new powerup will be the jumping or no clip powerup
    -- n random number to decide the powerup
    setPowerUp :: Int -> GameAction ()
    setPowerUp n = do
        (GameAttribute score isJump dogState _) <- getGameAttribute
        setGameAttribute (GameAttribute score isJump dogState (powerUpInfo $ modN n))
        where
            modN n = n `mod` (length powerUps)
            powerUpInfo n
                | n > 1 = jumpPowerInfo
                | otherwise = noClipPowerInfo

    -- reinitItem destroys an off-screen item and creates a new item
    -- item the current item to be destoyed
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

    -- reinitItems destroys a list of items and creates newly generated items
    -- (x:xs) list of items
    reinitItems :: [Item] -> GameAction ()    
    reinitItems [] = return ()
    reinitItems (x:xs) = do
        reinitItem x
        reinitItems xs

    -- itemCycle gets all currently loaded items from the game to destroy them and generate new items
    itemCycle :: GameAction ()
    itemCycle = do
        items <- getObjectsFromGroup "itemGroup"
        reinitItems items