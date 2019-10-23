{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

    import Graphics.UI.Fungen

    import Dog
    import Item
    import Graphics
    import Types

    -- refreshTime represents the time a new cycle is refreshed in ms
    refreshTime :: Int
    refreshTime = 60

    -- gameCycle handles all game actions for every cycle
    gameCycle :: GameAction ()
    gameCycle = do
        gameState <- getGameState
        (GameAttribute score _ _ _) <- getGameAttribute
        printOnScreen (show score) TimesRoman24 (1830,1160) 1.0 1.0 1.0
        case gameState of
            GameOver -> return ()
            GameCont -> do
                updateScore
                itemCycle
                dogCycle

    -- updateScore updates the score of the game
    updateScore :: GameAction ()
    updateScore = do
        (GameAttribute score isJump dogState powerUpInfo) <- getGameAttribute
        setGameAttribute (GameAttribute (score + 1) isJump dogState powerUpInfo)

    -- handlePress determines if the dog should jump or if the game should end based on game state
    handlePress :: Modifiers -> Position -> GameAction ()
    handlePress _ _ = do
        gameState <- getGameState
        case gameState of
            GameOver -> funExit
            GameCont -> jump

    -- main initializes the game hachi
    main :: IO ()
    main = do
        item <- createObstacle 0
        let winConfig = ((0, 0), windowSize, "hachi")
        let gameMap = textureMap (length pictures - 1) mapWidth mapHeight mapWidth mapHeight
        let dogGroup = objectGroup "dogGroup" [createDog]
        let itemGroup = objectGroup "itemGroup" [item]
        let initAttr = GameAttribute 0 False 0 (PowerUpInfo False NoPower (-1))
        let input = [(MouseButton LeftButton, Press, handlePress)]
            in funInit winConfig gameMap [dogGroup, itemGroup] GameCont initAttr input gameCycle (Timer refreshTime) pictures