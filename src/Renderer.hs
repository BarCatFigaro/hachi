module Renderer (render) where

    import           Data.Text  (Text)
    import qualified SDL
    import qualified SDL.Image

    render :: Text
            -> Int
            -> Int
            -> IO ()
    render title w h = do
        SDL.initialize [SDL.InitVideo]
        window <- SDL.createWindow title dw
        SDL.showWindow window

        screen <- SDL.getWindowSurface window
        image <- SDL.Image.load "./assets/dog/tile000.png"
        SDL.surfaceBlit image Nothing screen Nothing >> SDL.updateWindowSurface window

        SDL.delay 2000
        SDL.freeSurface image
        SDL.freeSurface screen

        SDL.destroyWindow window
        SDL.quit
        
        where
            dw = SDL.defaultWindow { SDL.windowInitialSize = z }
            z = SDL.V2 (fromIntegral w) (fromIntegral h)