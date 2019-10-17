module Types(
    dogCycle,
    jump,
    GameAction,
    GameAttribute(GameAttribute),
    Dog,
    maxHeight,
    Item,
    PowerUpInfo(PowerUpInfo),
    PowerUp(JumpPower, NoClipPower, NoPower),
    createDog,
    initPicture,
    windowSize,
    startPos,
    GameState(GameOver, GameCont),
) where

    import Graphics.UI.Fungen
    
    import Graphics

    type Dog = GameObject ()

    type Item = GameObject ()

    {- GameAttribute is one of:
        - score: Int, current score of the current game session
        - isJump: Bool, whether we are currently in the midst of jumping
        - dogState: Int, which dog state (in terms of sprite) we are in
        - powerUpInfo: PowerUpInfo
    -}
    data GameAttribute = GameAttribute Int Bool Int PowerUpInfo

    {- PowerUpInfo consists of:
        - Bool: whether we have hit a power up
        - PowerUp: the power up
        - Int: duration of the power up
     -}
    data PowerUpInfo = PowerUpInfo Bool PowerUp Int

    {- PowerUp is one of:
            - JumpPower
            - NoClipPower
        
        * each power up consists of:
            - power up value
    -}
    data PowerUp = NoPower | JumpPower Int | NoClipPower Int

    data GameState = GameOver | GameCont

    type GameAction a = IOGame GameAttribute () GameState () a

    windowSize :: (Int, Int)
    windowSize = (1920, 1200)

    initDogSize :: (Double, Double)
    initDogSize = (201, 195)

    dogSpeed :: Double
    dogSpeed = 40.0

    gravity :: Double
    gravity = 0.1

    maxHeight :: Double
    maxHeight = fromIntegral $ snd windowSize `div` 4

    extraJumpHeight :: Double
    extraJumpHeight = fromIntegral $ snd windowSize `div` 16

    startPos :: (Double, Double)
    startPos = (fromIntegral $ fst windowSize `div` 4, fromIntegral $ snd windowSize `div` 8)

    initPicture :: (Double, Double) -> Int -> ObjectPicture
    initPicture (x, y) = Tex (x, y)

    initDog :: String -> ObjectPicture -> Bool -> (Double, Double) -> (Double, Double) -> Dog
    initDog name pic isAsleep pos speed = object name pic isAsleep pos speed ()

    createDog :: Dog
    createDog = initDog "dog" (initPicture initDogSize 0)  False startPos (0, 0)

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
    handleCollision (x:xs) = do
        dog <- findObject "dog" "dogGroup"
        obsName <- getObjectName x
        hasCollided <- objectsCollision x dog
        when (hasCollided) (handleCollisionHelper dog obsName)

    handleCollisionHelper :: Dog -> String -> GameAction ()
    handleCollisionHelper dog name = do
        case name of
            "ball" -> do
                setObjectSpeed (0, 0) dog
                items <- getObjectsFromGroup "itemGroup"
                stopMovingObs items
                setGameState GameOver
            "power" -> do
                (GameAttribute score isJump dogState (PowerUpInfo _ powerUp duration)) <- getGameAttribute
                setGameAttribute (GameAttribute score isJump dogState (PowerUpInfo True powerUp duration))
                 
    handlePowerUp :: Dog -> PowerUp -> GameAction ()
    handlePowerUp dog powerUp = do
        (GameAttribute score isJump dogState (PowerUpInfo hasHit powerUp duration)) <- getGameAttribute
        let newDuration = duration - 1
        setGameAttribute (GameAttribute score isJump dogState (PowerUpInfo False powerUp newDuration))
        case powerUp of
            JumpPower val -> 
                return ()
            NoClipPower val ->
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
