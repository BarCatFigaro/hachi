module Types where

    import Graphics.UI.Fungen
    
    import Graphics

    -- Dog is a FunGen game object, representing the dog
    type Dog = GameObject ()

    -- Item is a FunGen game object, representing the obstacles and powerups
    type Item = GameObject ()

    {- GameAttribute is one of:
        - score: Int, current score of the current game session
        - isJump: Bool, whether we are currently in the midst of jumping
        - dogState: Int, which dog state (in terms of sprite) we are in
        - powerUpInfo: PowerUpInfo, stores the current powerup
    -}
    data GameAttribute = GameAttribute Int Bool Int PowerUpInfo

    {- PowerUpInfo consists of:
        - Bool: whether we have hit a power up
        - PowerUp: the type of power up
        - Int: duration of the power up
     -}
    data PowerUpInfo = PowerUpInfo Bool PowerUp Int

    {- PowerUp is one of:
            - NoPower
            - JumpPower
            - NoClipPower
    -}
    data PowerUp = NoPower | JumpPower | NoClipPower deriving Eq

    -- GameState represents the continuation or game over of the current game
    data GameState = GameOver | GameCont

    -- GameAction contains the game attributes and game state of the current game
    type GameAction a = IOGame GameAttribute () GameState () a

    -- windowSizeX is the Int width of the game window
    windowSizeX :: Int
    windowSizeX = 1920

    -- windowSizeY is the Int height of the game window
    windowSizeY :: Int
    windowSizeY = 1200

    -- mapWidth is the Double width of the game window
    mapWidth :: Double
    mapWidth = fromIntegral windowSizeX

    -- mapHeight is the Double height of the game window
    mapHeight :: Double
    mapHeight = fromIntegral windowSizeY

    -- windowSize is the window size of the current game
    windowSize :: (Int, Int)
    windowSize = (windowSizeX, windowSizeY)

    -- maxHeight is the maximum height permitted for all initialized items and dogs
    maxHeight :: Double
    maxHeight = fromIntegral $ snd windowSize `div` 4

    -- initPicture represents the picture of an object
    -- x the size of the picture
    -- y the index of the picture in pictures (from Graphics.hs)
    initPicture :: (Double, Double) -> Int -> ObjectPicture
    initPicture (x, y) = Tex (x, y)