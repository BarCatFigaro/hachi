{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

    import Graphics.UI.Fungen

    import Graphics
    import Types
    import Obstacle

    gameCycle :: GameAction ()
    gameCycle = do
        gameState <- getGameState
        case gameState of
            GameOver -> return ()
            GameCont -> do
                -- (GameAttribute score _ _) <- getGameAttribute --
                obstacle <- findObject "ball" "obstacleGroup"
                (_, pY) <- getObjectPosition obstacle
                printOnScreen (show pY) TimesRoman24 (0,0) 1.0 1.0 1.0
                dogCycle

    handlePress :: Modifiers -> Position -> GameAction ()
    handlePress m p = do
        gameState <- getGameState
        case gameState of
            GameOver -> funExit
            GameCont -> jump

    main :: IO ()
    main = do
        obstacles <- createObstacles 4
        let winConfig = ((0, 0), windowSize, "hachi")
        let gameMap = textureMap 8 1920 1200 1920.0 1200.0
        let dogGroup = objectGroup "dogGroup" [createDog]
        let obstacleGroup = objectGroup "obstacleGroup" obstacles
        let initAttr = GameAttribute 0 False 0
        let input = [(MouseButton LeftButton, Press, handlePress)]
            in funInit winConfig gameMap [dogGroup, obstacleGroup] GameCont initAttr input gameCycle (Timer 40) pictures