module Graphics where

    import Graphics.UI.Fungen

    colour :: Int -> Int -> Int -> InvList
    colour r g b = Just [(r, g, b)]
    
    magenta = colour 255 0 255
    pictures = [
        getDogPicture 0,
        getDogPicture 1,
        getDogPicture 2,
        getDogPicture 3,
        ("./assets/dog/tile004.bmp", magenta),
        ("./assets/world/background.bmp", Nothing)]

    getDogSize :: Int -> (Double, Double)
    getDogSize idx =
        let (_, _, size) = dogs !! idx
        in size

    getDogColour :: Int -> InvList
    getDogColour idx =
        let (_, col, _) = dogs !! idx
        in col

    getDogPath :: Int -> FilePath
    getDogPath idx =
        let (fp, _, _) = dogs !! idx
        in fp

    getDogPicture :: Int -> (FilePath, InvList)
    getDogPicture idx = (getDogPath idx, getDogColour idx)

    dogs = [
        ("./assets/dog/tile004.bmp", magenta, (201, 195)),
        ("./assets/dog/tile005.bmp", magenta, (201, 186)),
        ("./assets/dog/tile006.bmp", magenta, (201, 198)),
        ("./assets/dog/tile007.bmp", magenta, (207, 189))]