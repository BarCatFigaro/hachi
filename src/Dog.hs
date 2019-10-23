module Dog where

    import Graphics.UI.Fungen

    import Graphics
    import Item
    import Types

    -- dogSpeed speed of the dog
    dogSpeed :: Double
    dogSpeed = 40.0

    -- dogSize size of the dog
    dogSize :: (Double, Double)
    dogSize = (201, 195)

    -- gravity the speed a jumping dog falls back to the ground every cycle
    gravity :: Double
    gravity = 0.1

    -- extraJumpHeight the maximum height a jumping dog can reach on the screen
    extraJumpHeight :: Double
    extraJumpHeight = fromIntegral $ snd windowSize `div` 16

    -- startPos the starting position of the dog on the screen
    startPos :: (Double, Double)
    startPos = (fromIntegral $ fst windowSize `div` 4, fromIntegral $ snd windowSize `div` 8)

    -- initDog represents the starting dog
    -- name name of dog object
    -- pic dog sprite
    -- isAsleep true if dog is asleep; false otherwise
    -- pos x,y-position of the dog
    -- speed x velocity and y velocity of the dog
    initDog :: String -> ObjectPicture -> Bool -> (Double, Double) -> (Double, Double) -> Dog
    initDog name pic isAsleep pos speed = object name pic isAsleep pos speed ()

    -- createDog creates a new dog
    createDog :: Dog
    createDog = initDog "dog" (initPicture dogSize 0)  False startPos (0, 0)

    -- dogCycle handles all actions relating to dog, jumps and collisions
    dogCycle :: GameAction ()
    dogCycle = do
        dog <- findObject "dog" "dogGroup"
        items <- getObjectsFromGroup "itemGroup"
        (GameAttribute score isJump dogState (PowerUpInfo hasHit powerUp duration)) <- getGameAttribute
        (_, vY) <- getObjectSpeed dog
        let newDogState = (dogState + 1) `mod` (length dogs)
        setObjectCurrentPicture newDogState dog
        replaceObject dog $ updateObjectSize $ getPicSize newDogState dogs
        when (hasHit && duration > 0) (handlePowerUp dog powerUp)
        (GameAttribute _ _ _ powerUpInfo) <- getGameAttribute
        setGameAttribute $ GameAttribute score isJump newDogState powerUpInfo
        when isJump $ handleMotion dog vY
        handleCollision items

    -- handleCollision handles the collisions between a dog and an item
    -- (x:xs) the list of items (obstacles, powerups) in the game
    handleCollision :: [Item] -> GameAction ()
    handleCollision [] = return ()
    handleCollision (x:xs) = do
        dog <- findObject "dog" "dogGroup"
        obsName <- getObjectName x
        hasCollided <- objectsCollision x dog
        when (hasCollided) (handleCollisionHelper dog obsName x)

    -- handleCollisionHelper determines how to handle a collision, distinguishing between powerup and obstacle
    -- dog the dog in the game
    -- name the name of the item currently colliding with the dog
    -- item represents the item colliding with the dog
    handleCollisionHelper :: Dog -> String -> Item -> GameAction ()
    handleCollisionHelper dog name item = do
        (GameAttribute _ _ _ (PowerUpInfo hasHit powerUp duration)) <- getGameAttribute
        case name of
            "ball" -> do
                if (hasHit && powerUp == NoClipPower && duration > 0)
                    then setGameState GameCont
                    else do
                        setObjectSpeed (0, 0) dog
                        items <- getObjectsFromGroup "itemGroup"
                        stopMovingObs items
                        setGameState GameOver
            "power" -> do
                destroyObject item
                newItem <- createItem
                addObjectsToGroup [newItem] "itemGroup"                
                (GameAttribute score isJump dogState (PowerUpInfo _ powerUp duration)) <- getGameAttribute
                setGameAttribute (GameAttribute score isJump dogState (PowerUpInfo True powerUp duration))
    
    -- handlePowerUp updates the powerup for active state and duration left
    -- dog the dog in the game
    -- powerUp current powerup available in the game
    handlePowerUp :: Dog -> PowerUp -> GameAction ()
    handlePowerUp dog powerUp = do
        (GameAttribute score isJump dogState (PowerUpInfo _ powerUp duration)) <- getGameAttribute
        setGameAttribute (GameAttribute score isJump dogState (PowerUpInfo (duration > 0) powerUp (duration - 1)))

    -- stopMovingObs stops all items in the game
    -- (x:xs) list of all items in the game
    stopMovingObs :: [Item] -> GameAction ()
    stopMovingObs [] = return ()
    stopMovingObs (x:xs) = do
        setObjectSpeed (0, 0) x
        stopMovingObs xs

    -- handleMotion handles the jumping and falling of the dog
    -- dog the dog in the game
    -- vY y velocity of the dog
    handleMotion :: Dog -> Double -> GameAction ()
    handleMotion dog vY
            | vY > 0 = jumping dog
            | vY < 0 = falling dog
               
    -- jumping handles the jumping motion of the dog
    -- dog the dog in the game
    jumping :: Dog -> GameAction ()
    jumping dog = do
        (vX, vY) <- getObjectSpeed dog
        (_, pY) <- getObjectPosition dog
        (GameAttribute _ _ _ (PowerUpInfo hasHit powerUp _)) <- getGameAttribute
        if pY >= (maxHeight + jumpHeight hasHit powerUp)
            then reverseYSpeed dog
            else setObjectSpeed (vX, newSpeed vY) dog
            where
                jumpHeight hasHit powerUp
                    | hasHit && powerUp == JumpPower = 3 * extraJumpHeight
                    | otherwise = extraJumpHeight
                newSpeed vY
                    | vY - gravity > 0 = vY - gravity
                    | otherwise = vY

    -- falling handles the falling motion of the dog
    -- dog the dog in the game
    falling :: Dog -> GameAction ()
    falling dog = do
        (vX, vY) <- getObjectSpeed dog
        (_, pY) <- getObjectPosition dog
        if pY <= snd startPos
            then stop dog vX
            else setObjectSpeed (vX, vY - gravity) dog

    -- jump initializes the jumping motion of the dog if not already jumping
    jump :: GameAction ()
    jump = do
        (GameAttribute score _ dogState powerUpInfo) <- getGameAttribute
        setGameAttribute $ GameAttribute score True dogState powerUpInfo
        dog <- findObject "dog" "dogGroup"
        (vX, vY) <- getObjectSpeed dog
        setObjectSpeed (vX, newSpeed vY) dog
        where newSpeed vY
                | vY == 0 = dogSpeed
                | otherwise = vY

    -- stop stops the dog
    -- dog the dog in the game
    -- vX x velocity of the dog
    stop :: Dog -> Double -> GameAction ()
    stop dog vX = do
        (GameAttribute score _ dogState powerUpInfo) <- getGameAttribute
        setGameAttribute $ GameAttribute score False dogState powerUpInfo
        setObjectSpeed (vX, 0) dog
