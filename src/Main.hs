{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

    import Graphics.UI.Fungen

    import Graphics
    import Types

    gameCycle :: GameAction ()
    gameCycle = do
        (GameAttribute ga) <- getGameAttribute
        printOnScreen (show ga) TimesRoman24 (0,0) 1.0 1.0 1.0
        dog <- findObject "dog" "dogGroup"
        col1 <- objectLeftMapCollision dog
        col2 <- objectRightMapCollision dog
        when (col1 || col2) (reverseXSpeed dog)
        col3 <- objectTopMapCollision dog
        when col3 (reverseYSpeed dog)
        col4 <- objectBottomMapCollision dog
        when col4 (funExit)

    moveDogLeft :: Modifiers -> Position -> GameAction ()
    moveDogLeft m p = do
        obj <- findObject "dog" "dogGroup"
        (pX, pY) <- getObjectPosition obj
        (sX, _) <- getObjectSize obj
        if (pX - (sX/2) - 5 >= 0)
            then (setObjectPosition ((pX - 5),pY) obj)
            else (setObjectPosition (sX/2,pY) obj)


    moveDogRight :: Modifiers -> Position -> GameAction ()
    moveDogRight m p = do
        obj <- findObject "dog" "dogGroup"
        (pX, pY) <- getObjectPosition obj
        (sX, _) <- getObjectSize obj
        if (pX + (sX/2) + 5 <= 250)
            then (setObjectPosition ((pX + 5),pY) obj)
            else (setObjectPosition ((250 - (sX/2)),pY) obj)
        
    main :: IO ()
    main = let winConfig = ((0, 0), windowSize, "hachi")
               gameMap = textureMap 4 1920 1200 1920.0 1200.0
               dogGroup = objectGroup "dogGroup" [dog]
               initAttr = GameAttribute 5
               input = [(SpecialKey KeyLeft, StillDown, moveDogLeft),
                    (SpecialKey KeyRight, StillDown, moveDogRight)]
            in funInit winConfig gameMap [dogGroup] () initAttr input gameCycle (Timer 40) pictures