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
                updateScore
                (GameAttribute score _ _) <- getGameAttribute
                printOnScreen (show score) TimesRoman24 (1880,1160) 1.0 1.0 1.0
                obstacleCycle
                dogCycle

    updateScore :: GameAction ()
    updateScore = do
        (GameAttribute score isJump dogState) <- getGameAttribute
        setGameAttribute (GameAttribute (score + 1) isJump dogState)

    handlePress :: Modifiers -> Position -> GameAction ()
    handlePress m p = do
        gameState <- getGameState
        case gameState of
            GameOver -> funExit
            GameCont -> jump

    -- TODO handle long jump --
    handleLongPress :: Modifiers -> Position -> GameAction ()
    handleLongPress m p = do
        gameState <- getGameState
        case gameState of
            GameOver -> funExit
            GameCont -> jump

    main :: IO ()
    main = do
        obstacles <- createObstacles 1
        let winConfig = ((0, 0), windowSize, "hachi")
        let gameMap = textureMap (length pictures - 1) 1920 1200 1920.0 1200.0
        let dogGroup = objectGroup "dogGroup" [createDog]
        let obstacleGroup = objectGroup "obstacleGroup" obstacles
        let initAttr = GameAttribute 0 False 0
        let input = [(MouseButton LeftButton, Press, handlePress),
                     (MouseButton LeftButton, StillDown, handleLongPress)]
            in funInit winConfig gameMap [dogGroup, obstacleGroup] GameCont initAttr input gameCycle (Timer 100) pictures