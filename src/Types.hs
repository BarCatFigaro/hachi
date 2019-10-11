module Types where

    import Graphics.UI.Fungen
    
    import Graphics (pictures)

    type Dog = GameObject ()

    data GameAttribute = GameAttribute Int

    type GameAction a = IOGame GameAttribute () () () a

    windowSize :: (Int, Int)
    windowSize = (1920, 1200)

    dogSize :: (Double, Double)
    dogSize = (47, 67)

    startPos :: (Double, Double)
    startPos = (fromIntegral (fst windowSize `div` 2), fromIntegral (snd windowSize `div` 8))

    initDogPicture :: (Double, Double) -> Int -> ObjectPicture
    initDogPicture (x, y) = Tex (x, y)

    initDog :: String -> ObjectPicture -> Bool -> (Double, Double) -> (Double, Double) -> Dog
    initDog name pic isAsleep pos speed = object name pic isAsleep pos speed ()

    dog :: Dog
    dog = initDog "dog" (initDogPicture dogSize 0)  False startPos (0, 0)