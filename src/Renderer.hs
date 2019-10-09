{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Renderer (render) where

    import           Data.Text  (Text)
    import           Control.Monad.Extra (whileM)
    import           Control.Monad.IO.Class (MonadIO)
    import           Prelude hiding (Left, Right)

    import qualified SDL
    import qualified SDL.Image

    data Intent = SelectSurface Direction
                | Idle
                | Quit

    data Direction = None
                   | Up
                   | Down
                   | Left
                   | Right

    data SurfaceMap a = SurfaceMap {
        none  :: a,
        up    :: a,
        down  :: a,
        left  :: a,
        right :: a
    } deriving (Foldable, Traversable, Functor)

    surfacePaths :: SurfaceMap FilePath
    surfacePaths = SurfaceMap {
        none = "./assets/dog/tile019.png",
        up = "./assets/dog/tile008.png",
        down = "./assets/dog/tile000.png",
        left = "./assets/dog/tile012.png",
        right = "./assets/dog/tile004.png"
    }

    render :: Text
            -> Int
            -> Int
            -> IO ()
    render title w h = do
        SDL.initialize [SDL.InitVideo]
        window <- SDL.createWindow title dw
        SDL.showWindow window

        screen <- SDL.getWindowSurface window
        surfaces <- mapM SDL.Image.load surfacePaths
        
        let doRender = renderSurfaceToWindow window screen
        doRender (none surfaces)

        whileM $
            toIntent <$> SDL.pollEvent
            >>= runIntent surfaces doRender

        mapM_ SDL.freeSurface surfaces
        SDL.freeSurface screen

        SDL.destroyWindow window
        SDL.quit
        
        where
            dw = SDL.defaultWindow { SDL.windowInitialSize = z }
            z = SDL.V2 (fromIntegral w) (fromIntegral h)

    runIntent :: (Monad m) => SurfaceMap a -> (a -> m ()) -> Intent -> m Bool
    runIntent _ _ Quit = pure False
    runIntent _ _ Idle = pure True
    runIntent sp f (SelectSurface key) = True <$ f (selectSurface key sp)

    selectSurface :: Direction -> SurfaceMap a -> a
    selectSurface None = none
    selectSurface Up = up
    selectSurface Down = down
    selectSurface Left = left
    selectSurface Right = right

    toIntent :: Maybe SDL.Event -> Intent
    toIntent = maybe Idle (payloadToIntent . extractPayload)

    extractPayload :: SDL.Event -> SDL.EventPayload
    extractPayload (SDL.Event _t p) = p

    payloadToIntent :: SDL.EventPayload -> Intent
    payloadToIntent SDL.QuitEvent = Quit
    payloadToIntent (SDL.KeyboardEvent k) = getKey k
    payloadToIntent _ = Idle

    getKey :: SDL.KeyboardEventData -> Intent
    getKey (SDL.KeyboardEventData _ SDL.Released _ _) = Idle
    getKey (SDL.KeyboardEventData _ SDL.Pressed True _) = Idle
    getKey (SDL.KeyboardEventData _ SDL.Pressed False keysym) =
        case SDL.keysymKeycode keysym of
            SDL.KeycodeEscape -> Quit
            SDL.KeycodeUp -> SelectSurface Up
            SDL.KeycodeDown -> SelectSurface Down
            SDL.KeycodeLeft -> SelectSurface Left
            SDL.KeycodeRight -> SelectSurface Right
            _t               -> SelectSurface None

    renderSurfaceToWindow :: (MonadIO m) => SDL.Window -> SDL.Surface -> SDL.Surface -> m ()
    renderSurfaceToWindow w s i = SDL.surfaceBlit i Nothing s Nothing >> SDL.updateWindowSurface w