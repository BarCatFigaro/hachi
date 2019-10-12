module Types(
    dogCycle,
    jump,
    GameAction,
    GameAttribute(GameAttribute),
    Dog,
    dog,
    windowSize
) where

    import Graphics.UI.Fungen
    
    import Graphics (pictures)

    type Dog = GameObject ()

    data GameAttribute = GameAttribute Int Bool

    type GameAction a = IOGame GameAttribute () () () a

    windowSize :: (Int, Int)
    windowSize = (1920, 1200)

    dogSize :: (Double, Double)
    dogSize = (192, 192)

    dogSpeed :: Double
    dogSpeed = 30.0

    gravity :: Double
    gravity = 1.0

    maxHeight :: Double
    maxHeight = fromIntegral (snd windowSize `div` 4)

    startPos :: (Double, Double)
    startPos = (fromIntegral (fst windowSize `div` 4), fromIntegral (snd windowSize `div` 8))

    initDogPicture :: (Double, Double) -> Int -> ObjectPicture
    initDogPicture (x, y) = Tex (x, y)

    initDog :: String -> ObjectPicture -> Bool -> (Double, Double) -> (Double, Double) -> Dog
    initDog name pic isAsleep pos speed = object name pic isAsleep pos speed ()

    dog :: Dog
    dog = initDog "dog" (initDogPicture dogSize 0)  False startPos (0, 0)


    dogCycle :: Bool -> GameAction ()
    dogCycle isJump = do
        dog <- findObject "dog" "dogGroup"
        (GameAttribute score isJump) <- getGameAttribute
        (_, vY) <- getObjectSpeed dog
        when (isJump) (handleMotion dog vY)

    handleMotion :: Dog -> Double -> GameAction ()
    handleMotion dog vY
            | vY > 0 = jumping dog
            | vY < 0 = falling dog
                     

    jumping :: Dog -> GameAction ()
    jumping dog = do
        (vX, vY) <- getObjectSpeed dog
        (_, pY) <- getObjectPosition dog
        if (pY >= maxHeight)
            then reverseYSpeed dog
            else setObjectSpeed (vX, newSpeed vY) dog
            where newSpeed vY
                    | vY - gravity > 0 = vY - gravity
                    | otherwise = vY


    falling :: Dog -> GameAction ()
    falling dog = do
        (vX, vY) <- getObjectSpeed dog
        (_, pY) <- getObjectPosition dog
        if (pY <= snd startPos)
            then stop dog vX
            else setObjectSpeed (vX, vY - gravity) dog

    jump :: Modifiers -> Position -> GameAction ()
    jump _ _ = do
        (GameAttribute score _) <- getGameAttribute
        setGameAttribute (GameAttribute score True)
        dog <- findObject "dog" "dogGroup"
        (vX, vY) <- getObjectSpeed dog
        setObjectSpeed (vX, newSpeed vY) dog
        where newSpeed vY
                | vY == 0 = dogSpeed
                | otherwise = vY

    stop :: Dog -> Double -> GameAction ()
    stop dog vX = do
        (GameAttribute score _) <- getGameAttribute
        setGameAttribute (GameAttribute score False)
        setObjectSpeed (vX, 0) dog
