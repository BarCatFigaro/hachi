module Types where

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
            - NoPower
            - JumpPower
            - NoClipPower
    -}
    data PowerUp = NoPower | JumpPower | NoClipPower deriving Eq

    data GameState = GameOver | GameCont

    type GameAction a = IOGame GameAttribute () GameState () a

    windowSizeX :: Int
    windowSizeX = 1920

    windowSizeY :: Int
    windowSizeY = 1200

    mapWidth :: Double
    mapWidth = fromIntegral windowSizeX

    mapHeight :: Double
    mapHeight = fromIntegral windowSizeY

    windowSize :: (Int, Int)
    windowSize = (windowSizeX, windowSizeY)

    maxHeight :: Double
    maxHeight = fromIntegral $ snd windowSize `div` 4

    initPicture :: (Double, Double) -> Int -> ObjectPicture
    initPicture (x, y) = Tex (x, y)