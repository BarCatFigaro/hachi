module Renderer (orchestrate) where

    import           Data.Text  (Text)
    import           Linear
    import           Linear.Affine
    import           Foreign.C.Types
    import           SDL             (($=))
    import qualified SDL
    import qualified SDL.Image

    import           Game

    renderDog :: SDL.Renderer -> Assets -> Dog -> IO ()
    renderDog renderer assets dog = renderAsset renderer dogSprite cxy
            where
                dp = round $ dogPos dog
                ds = round $ dogState dog :: Int
                cxy = P (V2 50 dp)
                dogSprite = selectDogState (ds `mod` 4) assets

    renderAsset :: SDL.Renderer -> Asset -> Point V2 CInt -> IO ()
    renderAsset renderer (Asset texture size) xy = SDL.copy renderer texture Nothing (Just $ SDL.Rectangle xy size)

    renderAll :: SDL.Renderer -> Assets -> Game -> IO ()
    renderAll renderer assets game = do
        renderDog renderer assets (dog game)

    orchestrate :: Text
                 -> Int
                 -> Int
                 -> IO ()
    orchestrate title w h = do
        SDL.initialize [SDL.InitVideo]
        SDL.HintRenderScaleQuality $= SDL.ScaleNearest
        window <- SDL.createWindow title SDL.defaultWindow
        SDL.showWindow window
        renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
        assets <- loadAssets renderer

        -- TODO render --
        renderAll renderer assets initGame

        SDL.present renderer
        SDL.delay 5000

        destroyAssets assets
        SDL.destroyRenderer renderer
        SDL.quit

    loadAsset :: SDL.Renderer -> FilePath -> IO Asset
    loadAsset renderer filePath = do
        surface <- SDL.Image.load filePath
        texture <- SDL.createTextureFromSurface renderer surface
        size <- SDL.surfaceDimensions surface
        SDL.freeSurface surface
        return $ Asset texture size

    loadAssets :: SDL.Renderer -> IO Assets
    loadAssets renderer = Assets <$> loadAsset renderer "./assets/dog/tile008.png"
                                 <*> loadAsset renderer "./assets/dog/tile000.png"
                                 <*> loadAsset renderer "./assets/dog/tile012.png"
                                 <*> loadAsset renderer "./assets/dog/tile004.png"

    destroyAssets :: Assets -> IO ()
    destroyAssets assets = do
        SDL.destroyTexture $ texture $ dogUp assets
        SDL.destroyTexture $ texture $ dogDown assets
        SDL.destroyTexture $ texture $ dogLeft assets
        SDL.destroyTexture $ texture $ dogRight assets