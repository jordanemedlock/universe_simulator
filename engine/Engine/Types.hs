{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FunctionalDependencies #-}
module Engine.Types where

import Linear
import Graphics.Rendering.OpenGL hiding (Color)
import qualified Graphics.UI.GLFW as GLFW
import Data.Foldable
import Control.Lens.TH
import GHC.Int

data MeshType = TriangleArray | ElementsArray deriving Show

data MeshAsset = MeshAsset { _meshAssetVao :: VertexArrayObject 
                           , _meshAssetNumTris :: Int32
                           , _meshAssetMeshType :: MeshType
                           } deriving Show
makeFields ''MeshAsset

data Event =  KeyEvent GLFW.Key Int GLFW.KeyState GLFW.ModifierKeys
            | CursorEvent Double Double 
            | CharEvent Char 
            deriving Show

instance UniformComponent a => Uniform (V1 a) where 
    uniform loc = makeStateVar getter setter
        where
            getter = (\(Vector1 x) -> V1 x) <$> get (uniform loc)
            setter (V1 x) = uniform loc $= Vector1 x

instance UniformComponent a => Uniform (V2 a) where 
    uniform loc = makeStateVar getter setter
        where
            getter = (\(Vector2 x y) -> V2 x y) <$> get (uniform loc)
            setter (V2 x y) = uniform loc $= Vector2 x y

instance UniformComponent a => Uniform (V3 a) where 
    uniform loc = makeStateVar getter setter
        where
            getter = (\(Vector3 x y z) -> V3 x y z) <$> get (uniform loc)
            setter (V3 x y z) = uniform loc $= Vector3 x y z

instance {-# OVERLAPS #-} (MatrixComponent a, UniformComponent a) => Uniform (M44 a) where 
    uniform loc = makeStateVar (m44Getter loc) (m44Setter loc)

instance UniformComponent a => Uniform (V4 a) where 
    uniform loc = makeStateVar getter setter
        where
            getter = (\(Vector4 x y z w) -> V4 x y z w) <$> get (uniform loc)
            setter (V4 x y z w) = uniform loc $= Vector4 x y z w



m44Getter :: forall a. (MatrixComponent a, UniformComponent a) => UniformLocation -> IO (M44 a)
m44Getter loc = do
    mat <- get (uniform loc) :: IO (GLmatrix a)
    components <- getMatrixComponents RowMajor mat
    return $ toM44 components


m44Setter :: forall a. (MatrixComponent a, UniformComponent a) => UniformLocation -> M44 a -> IO () 
m44Setter loc mat = do
    let l = foldr (\i a -> toList i <> a) [] mat
    mat <- newMatrix RowMajor l :: IO (GLmatrix a)
    uniform loc $= (mat :: GLmatrix a)

toM44 :: [a] -> M44 a
toM44 mat = V4 (toV4 row1) (toV4 row2) (toV4 row3) (toV4 row4)
    where 
        (row1, rest1) = splitAt 4 mat
        (row2, rest2) = splitAt 4 rest1
        (row3, rest3) = splitAt 4 rest2
        (row4, []) = splitAt 4 rest3

toV4 :: [a] -> V4 a 
toV4 [x, y, z, w] = V4 x y z w