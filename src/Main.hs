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
                (GameAttribute score _ _) <- getGameAttribute
                printOnScreen (show score) TimesRoman24 (0,0) 1.0 1.0 1.0
                dogCycle

    handlePress :: Modifiers -> Position -> GameAction ()
    handlePress m p = do
        gameState <- getGameState
        case gameState of
            GameOver -> funExit
            GameCont -> jump

    main :: IO ()
    main = let winConfig = ((0, 0), windowSize, "hachi")
               gameMap = textureMap 5 1920 1200 1920.0 1200.0
               dogGroup = objectGroup "dogGroup" [dog]
               obstacleGroup = objectGroup "obstacleGroup" [obstacle]
               initAttr = GameAttribute 0 False 0
               input = [(MouseButton LeftButton, Press, handlePress)]
            in funInit winConfig gameMap [dogGroup, obstacleGroup] GameCont initAttr input gameCycle (Timer 40) pictures