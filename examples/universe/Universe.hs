{-# LANGUAGE MultiWayIf #-}
module Universe where

import Control.Monad.Random
import Engine.Apecs
import qualified Data.HashMap.Strict as HM
import qualified Data.PQueue.Min as Q
import Linear

newtype Seed = Seed Int deriving Show

newtype Universe = Universe String deriving Show
newtype Galaxy = Galaxy String deriving Show
newtype SolarSystem = SolarSystem String deriving Show
newtype Sun = Sun String deriving Show
newtype Planet = Planet String deriving Show
data Biome = Biome deriving Show

type Grid3 a = HM.HashMap (V3 Int) a
data Show a => QElem a = QElem Int a deriving Show
instance Show a => Eq (QElem a) where
    (QElem k1 _) == (QElem k2 _) = k1 == k2
instance Show a => Ord (QElem a) where
    (QElem k1 _) <= (QElem k2 _) = k1 <= k2

makeUniverse :: Int -> String -> (Universe, Seed)
makeUniverse seed name = (Universe name, Seed seed)

gridCoord :: Double -> V3 Double -> V3 Int
gridCoord cellSize = (floor.(/cellSize) <$>)

mkQElem :: Monad m => Show a => a -> RandT StdGen m (QElem a)
mkQElem x = do
    r <- getRandom
    return $ QElem r x

generateRandomPointAround :: Monad m => V3 Double -> Double -> RandT StdGen m (V3 Double)
generateRandomPointAround point minDist = do
    r1 <- getRandomR (0, 1)
    r2 <- getRandomR (0, 1)
    r3 <- getRandomR (0, 1)
    
    let radius = minDist * (r1 + 1)
    let theta = 2 * pi * r2
    let phi = 2 * pi * r3
    return $ point + radius *^ V3 (cos theta * sin phi) (sin theta * sin phi) (cos phi)


makeGalaxies :: MonadIO m => Int -> V3 Double -> (V3 Double -> Double) -> RandT StdGen m [(Galaxy, Seed, Pos)]
makeGalaxies n size minDistF = do

    points <- poissonDiskPoints n size minDistF

    names <- ("Milky Way" :).(<>["Corlock"]) <$> mapM (const galaxyName) [0..n-3]

    seeds <- getRandoms

    return $ zip3 (Galaxy <$> names) (Seed <$> seeds) (Pos <$> points)


poissonDiskPoints :: MonadIO m => Int -> V3 Double -> (V3 Double -> Double) -> RandT StdGen m [V3 Double]
poissonDiskPoints n size minDistF = do
    pos <- getRandomR (0, size)

    qelem <- mkQElem pos

    let cellSize = minDistF pos / sqrt 2

    let grid = HM.singleton (gridCoord cellSize pos) pos
    let processList = Q.singleton qelem
    let samplePoints = [pos]

    

    let helper point (grid, processList, samplePoints) i = do
            newPoint <- generateRandomPointAround point (minDistF point)

            if not $ inNeighborhood grid newPoint (minDistF newPoint) cellSize
                then do 
                    qelem <- mkQElem newPoint
                    let processList' = Q.insert qelem processList
                    let samplePoints' = newPoint : samplePoints
                    let grid' = HM.insert (gridCoord cellSize newPoint) newPoint grid
                    return (grid', processList', samplePoints')
                else return (grid, processList, samplePoints)

    let loop grid processList samplePoints = 
            if Q.null processList || length samplePoints >= n
            then return samplePoints
            else do
                let (QElem _ point, processList') = Q.deleteFindMin processList

                (grid', processList'', samplePoints') <- foldM (helper point) (grid, processList', samplePoints) ([1..n] :: [Int])

                loop grid' processList'' samplePoints'
    take n <$> loop grid processList samplePoints

inNeighborhood grid point minDist cellSize = any helper cellsAroundPoint
    where 
        gridPoint = gridCoord cellSize point
        cellsAroundPoint = squareAroundPoint gridPoint 5
        helper cell = norm (cell - point) < minDist

squareAroundPoint (V3 px py pz) n = [ V3 (realToFrac $ px + x) (realToFrac $ py + y) (realToFrac $ pz + z)
                                    | x <- [-(n `div` 2)..(n `div` 2)]
                                    , y <- [-(n `div` 2)..(n `div` 2)]
                                    , z <- [-(n `div` 2)..(n `div` 2)]
                                    ]

nm1 = ["Alpha", "Apus", "Aquila", "Ara", "Beta", "Canis", "Carina", "Comae", "Corona", "Crux", "Delta", "Draco", "Epsilon", "Gamma", "Lambda", "Lyra", "Nemo", "Omega", "Omicron", "Pavo", "Proxima", "Sagitta", "Serpens", "Sigma", "Theta", "Upsilon", "Ursa", "Vela", "Virgo", "Zeta", "", "", "", "", "", ""]
nm2 = ["Acallaris", "Achelois", "Adastreia", "Aegialeus", "Aegimius", "Alatheia", "Alcyoneus", "Aldebaran", "Amphiaraus", "Anadeia", "Andromeda", "Aquarii", "Arcturus", "Aristaeus", "Asteria", "Asteropaios", "Astraeus", "Aurigae", "Boreas", "Borysthenis", "Calesius", "Capella", "Cassiopeia", "Centauri", "Centaurus", "Chronos", "Cymopoleia", "Dioscuri", "Draconis", "Eioneus", "Eridani", "Eridanus", "Eubuleus", "Euphorion", "Eusebeia", "Euthenia", "Hemithea", "Hyperbius", "Hyperes", "Hyperion", "Icarius", "Ichnaea", "Ilioneus", "Kentaurus", "Leporis", "Librae", "Lyrae", "Majoris", "Miriandynus", "Myrmidon", "Nebula", "Nemesis", "Odysseus", "Ophiuchi", "Orion", "Orionis", "Orithyia", "Palioxis", "Peleus", "Perileos", "Perseus", "Phoroneus", "Polystratus", "Porphyrion", "Proioxis", "Sagittarius", "Sirius", "Solymus", "Zagreus", "Zephyrus"]
nm3 = ["Abyss", "Acorn", "Arrowhead", "Banana", "Beansprout", "Beanstalk", "Bell", "Blue Ribbon", "Blueberry", "Bottleneck", "Bowl", "Bull's Eye", "Bullet", "Butterfly", "Cat's Ear", "Cat's Eye", "Catterpillar", "Cherry", "Chickpea", "Clover", "Coconut", "Comet", "Crescent", "Crow's Feet", "Crown", "Dandelion", "Diamond", "Dragontooth", "Droplet", "Eagle Claw", "Eggshell", "Exploding", "Eyebrow", "Eyelash", "Falling", "Feather", "Fern Leaf", "Fingerprint", "Fisheye", "Fishscale", "Flame", "Football", "Grain", "Halo", "Heart", "Horseshoe", "Hurricane", "Icicle", "Iris", "Jellyfish", "Kettle", "Leaf", "Lemon", "Lightbulb", "Lilypad", "Lion's Mane", "Lion's Tail", "Maelstrom", "Meridian", "Mosaic", "Mouse", "Octopus", "Oculus", "Onion", "Owl Head", "Pear", "Pepper", "Pig's Tail", "Pinecone", "Ponytail", "Potato", "Red Ribbon", "Rippled", "Rose Petal", "Sawblade", "Seashell", "Serpent", "Serpent's Eye", "Sharkfin", "Sharktooth", "Shield", "Shooting Star", "Snail Shell", "Snowflake", "Soap Bubble", "Sparrow", "Spearpoint", "Spiderleg", "Spiderweb", "Spiral", "Starfish", "Strawberry", "Teacup", "Tiara", "Tiger Paw", "Tree Root", "Tree Trunk", "Turtle Shell", "Vortex", "Wave", "Whale's Tail", "Zodiac"]
nm4 = ["Nebula", "Galaxy", "Cloud", "Star System"]
nm5 = ["A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z", ""]
nm6 = ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9", ""]

select :: Monad m => [a] -> RandT StdGen m a
select xs = do
    i <- getRandomR (0, length xs-1)
    return $ xs !! i

galaxyName :: Monad m => RandT StdGen m String
galaxyName = do
    i <- getRandomR (0 :: Int, 10)

    if  | i < 3 -> (\x y -> x<>" "<>y) <$> select nm1 <*> select nm2
        | i < 5 -> (\x y -> x<>" "<>y) <$> select nm2 <*> select nm4
        | i < 8 -> (\x y -> x<>" "<>y) <$> select nm3 <*> select nm4
        | i < 9 -> (\a b c d e -> a<>b<>"-"<>c<>d<>e) 
                            <$> select nm5 <*> select nm5 
                            <*> select nm6 <*> select nm6 <*> select nm6
        | otherwise -> (\a b c d e f -> a<>b<>c<>"-"<>d<>e<>f) 
                            <$> select nm5 <*> select nm5 <*> select nm5 
                            <*> select nm6 <*> select nm6 <*> select nm6



-- mkWhiteNoise :: Monad m => V3 Int -> RandT StdGen m (Grid3 Double)
-- mkWhiteNoise (V3 x y z) = foldM helper HM.empty $ V3 <$> [0..x-1] <*> [0..y-1] <*> [0..z-1]
--     where
--         helper hm v = do
--             r <- getRandomR (0, 1)
--             return $ HM.insert v r hm

-- mkSmoothNoise :: Monad m => V3 Int -> Grid3 Double -> Int -> RandT StdGen m (Grid3 Double)
-- mkSmoothNoise size@(V3 w h d) grid octave = do
--     let indices = V3 <$> [0..w-1] <*> [0..h-1] <*> [0..d-1]

--     let samplePeriod = 2 ** octave
--     let sampleFrequency = 1 / samplePeriod

--     let helper hm v@(V3 x y z) = do
--         let sample0@(V3 x0 y0 z0) = (v `div` samplePeriod) * samplePeriod
--         let sample1@(V3 x1 y1 z1) = (sample0 + samplePeriod) `mod` size
--         let blend@(V3 bx by bz) = (v - sample0) * sampleFrequency

--         let top = interpolate (baseNoise ! (V3 )) (baseNoise ! sample)



--     foldM helper HM.empty indices