module Dog where

    import Graphics.UI.Fungen

    import Graphics
    import Item
    import Types

    dogSpeed :: Double
    dogSpeed = 40.0

    dogSize :: (Double, Double)
    dogSize = (201, 195)

    gravity :: Double
    gravity = 0.1

    extraJumpHeight :: Double
    extraJumpHeight = fromIntegral $ snd windowSize `div` 16

    startPos :: (Double, Double)
    startPos = (fromIntegral $ fst windowSize `div` 4, fromIntegral $ snd windowSize `div` 8)

    initDog :: String -> ObjectPicture -> Bool -> (Double, Double) -> (Double, Double) -> Dog
    initDog name pic isAsleep pos speed = object name pic isAsleep pos speed ()

    createDog :: Dog
    createDog = initDog "dog" (initPicture dogSize 0)  False startPos (0, 0)

    dogCycle :: GameAction ()
    dogCycle = do
        dog <- findObject "dog" "dogGroup"
        items <- getObjectsFromGroup "itemGroup"
        -- TODO decide if duration is num of cycles vs. num of seconds --
        (GameAttribute score isJump dogState (PowerUpInfo hasHit powerUp duration)) <- getGameAttribute
        (_, vY) <- getObjectSpeed dog
        let newDogState = (dogState + 1) `mod` 4
        setObjectCurrentPicture newDogState dog
        replaceObject dog $ updateObjectSize $ getPicSize newDogState dogs
        when (hasHit && duration > 0) (handlePowerUp dog powerUp)
        (GameAttribute _ _ _ powerUpInfo) <- getGameAttribute
        setGameAttribute $ GameAttribute score isJump newDogState powerUpInfo
        when isJump $ handleMotion dog vY
        handleCollision items

    handleCollision :: [Item] -> GameAction ()
    handleCollision [] = return ()
    handleCollision (x:xs) = do
        dog <- findObject "dog" "dogGroup"
        obsName <- getObjectName x
        hasCollided <- objectsCollision x dog
        when (hasCollided) (handleCollisionHelper dog obsName x)

    handleCollisionHelper :: Dog -> String -> Item -> GameAction ()
    handleCollisionHelper dog name item = do
        case name of
            "ball" -> do
                setObjectSpeed (0, 0) dog
                items <- getObjectsFromGroup "itemGroup"
                stopMovingObs items
                setGameState GameOver
            "power" -> do
                destroyObject item
                item <- liftIOtoIOGame createItem
                addObjectsToGroup [item] "itemGroup"                
                (GameAttribute score isJump dogState (PowerUpInfo _ powerUp duration)) <- getGameAttribute
                setGameAttribute (GameAttribute score isJump dogState (PowerUpInfo True powerUp duration))
                 
    handlePowerUp :: Dog -> PowerUp -> GameAction ()
    handlePowerUp dog powerUp = do
        (GameAttribute score isJump dogState (PowerUpInfo _ powerUp duration)) <- getGameAttribute
        setGameAttribute (GameAttribute score isJump dogState (PowerUpInfo (duration > 0) powerUp (duration - 1)))
        case powerUp of
            JumpPower -> 
                return ()
            NoClipPower ->
                return ()
            NoPower ->
                return ()

    stopMovingObs :: [Item] -> GameAction ()
    stopMovingObs [] = return ()
    stopMovingObs (x:xs) = do
        setObjectSpeed (0, 0) x
        stopMovingObs xs

    handleMotion :: Dog -> Double -> GameAction ()
    handleMotion dog vY
            | vY > 0 = jumping dog
            | vY < 0 = falling dog
                     
    jumping :: Dog -> GameAction ()
    jumping dog = do
        (vX, vY) <- getObjectSpeed dog
        (_, pY) <- getObjectPosition dog
        if pY >= (maxHeight + extraJumpHeight)
            then reverseYSpeed dog
            else setObjectSpeed (vX, newSpeed vY) dog
            where newSpeed vY
                    | vY - gravity > 0 = vY - gravity
                    | otherwise = vY

    falling :: Dog -> GameAction ()
    falling dog = do
        (vX, vY) <- getObjectSpeed dog
        (_, pY) <- getObjectPosition dog
        if pY <= snd startPos
            then stop dog vX
            else setObjectSpeed (vX, vY - gravity) dog

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

    stop :: Dog -> Double -> GameAction ()
    stop dog vX = do
        (GameAttribute score _ dogState powerUpInfo) <- getGameAttribute
        setGameAttribute $ GameAttribute score False dogState powerUpInfo
        setObjectSpeed (vX, 0) dog
