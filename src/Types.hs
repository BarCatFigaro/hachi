module Types(
    dogCycle,
    jump,
    GameAction,
    GameAttribute(GameAttribute),
    Dog,
    maxHeight,
    Obstacle,
    PowerUp,
    createDog,
    initPicture,
    windowSize,
    startPos,
    GameState(GameOver, GameCont),
) where

    import Graphics.UI.Fungen
    
    import Graphics

    type Dog = GameObject ()

    type Obstacle = GameObject ()

    type PowerUp = GameObject ()

    data GameAttribute = GameAttribute Int Bool Int

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
        obstacles <- getObjectsFromGroup "obstacleGroup"
        (GameAttribute score isJump dogState) <- getGameAttribute
        (_, vY) <- getObjectSpeed dog
        setObjectCurrentPicture ((dogState + 1) `mod` 4) dog
        replaceObject dog $ updateObjectSize $ getPicSize ((dogState + 1) `mod` 4) dogs
        setGameAttribute $ GameAttribute score isJump ((dogState + 1) `mod` 4)
        when isJump $ handleMotion dog vY
        handleCollision obstacles

    handleCollision :: [Obstacle] -> GameAction ()
    handleCollision (x:xs) = do
        dog <- findObject "dog" "dogGroup"
        obsName <- getObjectName x
        hasCollided <- objectsCollision x dog
        when (hasCollided) (handleCollisionHelper dog obsName)

    handleCollisionHelper :: Dog -> String -> GameAction ()
    handleCollisionHelper dog name = do
        case name of
            obstacleName -> do
                setObjectSpeed (0, 0) dog
                obstacles <- getObjectsFromGroup "obstacleGroup"
                stopMovingObs obstacles
                setGameState GameOver
            powerUpName -> do
                (vX, vY) <- getObjectSpeed dog
                setObjectSpeed (vX, vY - 100) dog

    stopMovingObs :: [Obstacle] -> GameAction ()
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
        (GameAttribute score _ dogState) <- getGameAttribute
        setGameAttribute $ GameAttribute score True dogState
        dog <- findObject "dog" "dogGroup"
        (vX, vY) <- getObjectSpeed dog
        setObjectSpeed (vX, newSpeed vY) dog
        where newSpeed vY
                | vY == 0 = dogSpeed
                | otherwise = vY

    stop :: Dog -> Double -> GameAction ()
    stop dog vX = do
        (GameAttribute score _ dogState) <- getGameAttribute
        setGameAttribute $ GameAttribute score False dogState
        setObjectSpeed (vX, 0) dog
