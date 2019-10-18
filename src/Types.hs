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
    data PowerUp = NoPower | JumpPower | NoClipPower

    data GameState = GameOver | GameCont

    type GameAction a = IOGame GameAttribute () GameState () a

    windowSize :: (Int, Int)
    windowSize = (1920, 1200)

    maxHeight :: Double
    maxHeight = fromIntegral $ snd windowSize `div` 4

    initPicture :: (Double, Double) -> Int -> ObjectPicture
    initPicture (x, y) = Tex (x, y)