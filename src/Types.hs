module Types(
    dogCycle,
    jump,
    GameAction,
    GameAttribute(GameAttribute),
    Dog,
    maxHeight,
    Obstacle,
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

    data GameAttribute = GameAttribute Int Bool Int

    data GameState = GameOver | GameCont

    type GameAction a = IOGame GameAttribute () GameState () a

    windowSize :: (Int, Int)
    windowSize = (1920, 1200)

    initDogSize :: (Double, Double)
    initDogSize = (201, 195)

    dogSpeed :: Double
    dogSpeed = 30.0

    gravity :: Double
    gravity = 1.0

    maxHeight :: Double
    maxHeight = fromIntegral $ snd windowSize `div` 4

    startPos :: (Double, Double)
    startPos = (fromIntegral $ fst windowSize `div` 4, fromIntegral $snd windowSize `div` 8)

    initPicture :: (Double, Double) -> Int -> ObjectPicture
    initPicture (x, y) = Tex (x, y)

    initDog :: String -> ObjectPicture -> Bool -> (Double, Double) -> (Double, Double) -> Dog
    initDog name pic isAsleep pos speed = object name pic isAsleep pos speed ()

    createDog :: Dog
    createDog = initDog "dog" (initPicture initDogSize 0)  False startPos (0, 0)

    dogCycle :: GameAction ()
    dogCycle = do
        dog <- findObject "dog" "dogGroup"
        (GameAttribute score isJump dogState) <- getGameAttribute
        (_, vY) <- getObjectSpeed dog
        setObjectCurrentPicture ((dogState + 1) `mod` 4) dog
        replaceObject dog $ updateObjectSize $ getPicSize ((dogState + 1) `mod` 4) dogs
        setGameAttribute $ GameAttribute score isJump ((dogState + 1) `mod` 4)
        when isJump $ handleMotion dog vY
        handleCollision

    handleCollision :: GameAction ()
    handleCollision = do
        dog <- findObject "dog" "dogGroup"
        obstacles <- getObjectsFromGroup "obstacleGroup"
        obstacleCollision <- objectListObjectCollision obstacles dog
        when
            obstacleCollision
            (do setObjectSpeed (0, 0) dog
                stopMovingObs obstacles
                setGameState GameOver)

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
        if pY >= maxHeight
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
