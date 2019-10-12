{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

    import Graphics.UI.Fungen

    import Graphics
    import Types

    gameCycle :: GameAction ()
    gameCycle = do
        (GameAttribute ga gb) <- getGameAttribute
        printOnScreen (show ga) TimesRoman24 (0,0) 1.0 1.0 1.0
        dogCycle gb
        -- col1 <- objectLeftMapCollision dog --
        -- col2 <- objectRightMapCollision dog --
        -- when (col1 || col2) (reverseXSpeed dog) --
        -- col3 <- objectTopMapCollision dog --
        -- when col3 (reverseYSpeed dog) --
        
    main :: IO ()
    main = let winConfig = ((0, 0), windowSize, "hachi")
               gameMap = textureMap 4 1920 1200 1920.0 1200.0
               dogGroup = objectGroup "dogGroup" [dog]
               initAttr = GameAttribute 0 False
               input = [(MouseButton LeftButton, Press, jump)]
            in funInit winConfig gameMap [dogGroup] () initAttr input gameCycle (Timer 40) pictures