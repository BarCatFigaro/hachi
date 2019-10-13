module Graphics where

    import Graphics.UI.Fungen

    color :: Int -> Int -> Int -> InvList
    color r g b = Just [(r, g, b)]
    
    magenta = color 255 0 255
    pictures = [
        ("./assets/dog/tile004.bmp", magenta),
        ("./assets/dog/tile005.bmp", magenta),
        ("./assets/dog/tile006.bmp", magenta),
        ("./assets/dog/tile007.bmp", magenta),
        ("./assets/dog/tile004.bmp", magenta),
        ("./assets/world/background.bmp", Nothing)]