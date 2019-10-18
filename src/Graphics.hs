module Graphics where

    import Graphics.UI.Fungen

    type InternalPicture = (FilePath, InvList, (Double, Double))

    colour :: Int -> Int -> Int -> InvList
    colour r g b = Just [(r, g, b)]
    
    magenta = colour 255 0 255
    pictures = [
        getPicture 0 dogs,
        getPicture 1 dogs,
        getPicture 2 dogs,
        getPicture 3 dogs,
        getPicture 0 obstacles,
        getPicture 1 obstacles,
        getPicture 2 obstacles,
        getPicture 3 obstacles,
        getPicture 0 powerUps,
        getPicture 1 powerUps,
        getPicture 2 powerUps,
        getPicture 3 powerUps,
        ("./assets/world/background.bmp", Nothing)]

    dogs :: [InternalPicture]
    dogs = [
        ("./assets/dog/tile004.bmp", magenta, (201, 195)),
        ("./assets/dog/tile005.bmp", magenta, (201, 186)),
        ("./assets/dog/tile006.bmp", magenta, (201, 198)),
        ("./assets/dog/tile007.bmp", magenta, (207, 189))]

    obstacles :: [InternalPicture]
    obstacles = [
        ("./assets/objects/tile379.bmp", magenta, (45, 45)),
        ("./assets/objects/tile379.bmp", magenta, (45, 45)),
        ("./assets/objects/tile379.bmp", magenta, (45, 45)),
        ("./assets/objects/tile379.bmp", magenta, (45, 45))]

    powerUps :: [InternalPicture]
    powerUps = [
        ("./assets/objects/tile026.bmp", magenta, (45, 45)),
        ("./assets/objects/tile026.bmp", magenta, (45, 45)),
        ("./assets/objects/tile040.bmp", magenta, (45, 45)),
        ("./assets/objects/tile040.bmp", magenta, (45, 45))]

    getPicSize :: Int -> [InternalPicture] -> (Double, Double)
    getPicSize idx pics =
        let (_, _, size) = pics !! idx
        in size

    getPicColour :: Int -> [InternalPicture] -> InvList
    getPicColour idx pics =
        let (_, col, _) = pics !! idx
        in col

    getPicPath :: Int -> [InternalPicture] -> FilePath
    getPicPath idx pics =
        let (fp, _, _) = pics !! idx
        in fp

    getPicture :: Int -> [InternalPicture] -> (FilePath, InvList)
    getPicture idx pics = (getPicPath idx pics, getPicColour idx pics)
