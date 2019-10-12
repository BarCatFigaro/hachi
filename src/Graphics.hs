module Graphics where

    import Graphics.UI.Fungen

    color :: Int -> Int -> Int -> InvList
    color r g b = Just [(r, g, b)]
    
    magenta = color 255 0 255
    pictures = [
        ("./assets/dog/tile004.bmp", magenta),
        ("./assets/dog/tile000.bmp", Nothing),
        ("./assets/dog/tile012.bmp", Nothing),
        ("./assets/dog/tile004.bmp", Nothing),
        ("./assets/world/background.bmp", Nothing)]