{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

    import Renderer

    main :: IO ()
    main = do
        putStrLn "Henlo Scala"
        orchestrate "Henlo Lambda" 640 480