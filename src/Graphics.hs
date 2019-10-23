module Graphics where

    import Graphics.UI.Fungen

    -- InternalPicture has a file path, a list of colours not rendered, and the size of the picture
    type InternalPicture = (FilePath, InvList, (Double, Double))

    -- colour takes in an RGB value and returns a list of colours (to pass into InternalPicture)
    -- r the R value
    -- g the g value
    -- b the b value
    colour :: Int -> Int -> Int -> InvList
    colour r g b = Just [(r, g, b)]
    
    -- magenta is the colour magenta; type InvList
    magenta = colour 255 0 255

    -- pictures is a list of all the sprites used in hachi
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

    -- dogs is a list of all dog sprites
    dogs :: [InternalPicture]
    dogs = [
        ("./assets/dog/tile004.bmp", magenta, (201, 195)),
        ("./assets/dog/tile005.bmp", magenta, (201, 186)),
        ("./assets/dog/tile006.bmp", magenta, (201, 198)),
        ("./assets/dog/tile007.bmp", magenta, (207, 189))]

    -- obstacles is a list of all obstacle sprites used in hachi
    -- ball
    obstacles :: [InternalPicture]
    obstacles = [
        ("./assets/objects/tile379.bmp", magenta, (45, 45)),
        ("./assets/objects/tile379.bmp", magenta, (45, 45)),
        ("./assets/objects/tile379.bmp", magenta, (45, 45)),
        ("./assets/objects/tile379.bmp", magenta, (45, 45))]

    -- powerUps is a list of all powerup sprites used in hachi
    -- blushing happy face; no clip power
    -- smiling face; jumping power
    powerUps :: [InternalPicture]
    powerUps = [
        ("./assets/objects/tile026.bmp", magenta, (45, 45)),
        ("./assets/objects/tile026.bmp", magenta, (45, 45)),
        ("./assets/objects/tile040.bmp", magenta, (45, 45)),
        ("./assets/objects/tile040.bmp", magenta, (45, 45))]

    -- getPicSize returns the picture size of a sprite
    -- idx index of sprite in a list of pictures
    -- pics the list of pictures
    getPicSize :: Int -> [InternalPicture] -> (Double, Double)
    getPicSize idx pics =
        let (_, _, size) = pics !! idx
        in size

    -- getPicColour returns the InvList (not rendered colours) of a sprite
    -- idx index of sprite in a list of pictures
    -- pics the list of pictures
    getPicColour :: Int -> [InternalPicture] -> InvList
    getPicColour idx pics =
        let (_, col, _) = pics !! idx
        in col

    -- getPicPath returns the file path of a sprite
    -- idx index of sprite in a list of pictures
    -- pics the list of pictures
    getPicPath :: Int -> [InternalPicture] -> FilePath
    getPicPath idx pics =
        let (fp, _, _) = pics !! idx
        in fp

    -- getPicture returns the file path and InvList (not rendered colours) of a sprite
    -- idx index of sprite in a list of pictures
    -- pics the list of pictures
    getPicture :: Int -> [InternalPicture] -> (FilePath, InvList)
    getPicture idx pics = (getPicPath idx pics, getPicColour idx pics)
