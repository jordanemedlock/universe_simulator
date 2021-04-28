
import Universe
import Control.Monad.Random
import Linear


main = do
    galaxies <- evalRandT (makeGalaxies 100 (V3 100 100 100) $ const 1) (mkStdGen 0)

    mapM_ print $ zip [1..] galaxies