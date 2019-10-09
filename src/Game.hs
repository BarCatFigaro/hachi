module Game where
    
    import qualified SDL
    import           Linear
    import           Foreign.C.Types

    data Dog = Dog {
        dogPos :: Double,
        dogState :: Double
    } deriving (Show)

    initDog :: Dog
    initDog = Dog { dogPos = 100.0, dogState = 0.0 }

    selectDogState :: Int -> Assets -> Asset
    selectDogState state assets = case state `mod` 4 of
        0 -> dogUp assets
        1 -> dogDown assets
        2 -> dogLeft assets
        3 -> dogRight assets
        _ -> dogUp assets

    data Game = Game {
        dog :: Dog
    } deriving (Show)

    initGame :: Game
    initGame = Game { dog = initDog }

    data Assets = Assets {
        dogUp :: Asset,
        dogDown :: Asset,
        dogLeft :: Asset,
        dogRight :: Asset
    }

    data Asset = Asset {
        texture :: SDL.Texture,
        size :: V2 CInt
    }