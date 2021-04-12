{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FunctionalDependencies #-}
module Engine.Types where

import Linear
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import Data.Foldable
import Control.Lens.TH
import GHC.Int

data MeshType = Arrays | Elements deriving Show

data MeshAsset = MeshAsset { meshVao :: GL.VertexArrayObject 
                           , meshNumValues :: Int32
                           , meshType :: MeshType
                           , meshMode :: GL.PrimitiveMode
                           , meshElementType :: GL.DataType
                           } deriving Show

data Event =  KeyEvent GLFW.Key Int GLFW.KeyState GLFW.ModifierKeys
            | CursorEvent Double Double 
            | CharEvent Char 
            deriving Show

instance GL.UniformComponent a => GL.Uniform (V1 a) where 
    uniform loc = GL.makeStateVar getter setter
        where
            getter = (\(GL.Vector1 x) -> V1 x) <$> GL.get (GL.uniform loc)
            setter (V1 x) = GL.uniform loc $= GL.Vector1 x

instance GL.UniformComponent a => GL.Uniform (V2 a) where 
    uniform loc = GL.makeStateVar getter setter
        where
            getter = (\(GL.Vector2 x y) -> V2 x y) <$> GL.get (GL.uniform loc)
            setter (V2 x y) = GL.uniform loc $= GL.Vector2 x y

instance GL.UniformComponent a => GL.Uniform (V3 a) where 
    uniform loc = GL.makeStateVar getter setter
        where
            getter = (\(GL.Vector3 x y z) -> V3 x y z) <$> GL.get (GL.uniform loc)
            setter (V3 x y z) = GL.uniform loc $= GL.Vector3 x y z

instance {-# OVERLAPS #-} (GL.MatrixComponent a, GL.UniformComponent a) => GL.Uniform (M44 a) where 
    uniform loc = GL.makeStateVar (m44Getter loc) (m44Setter loc)

instance GL.UniformComponent a => GL.Uniform (V4 a) where 
    uniform loc = GL.makeStateVar getter setter
        where
            getter = (\(GL.Vector4 x y z w) -> V4 x y z w) <$> GL.get (GL.uniform loc)
            setter (V4 x y z w) = GL.uniform loc $= GL.Vector4 x y z w



m44Getter :: forall a. (GL.MatrixComponent a, GL.UniformComponent a) => GL.UniformLocation -> IO (M44 a)
m44Getter loc = do
    mat <- GL.get (GL.uniform loc) :: IO (GL.GLmatrix a)
    components <- GL.getMatrixComponents GL.RowMajor mat
    return $ toM44 components


m44Setter :: forall a. (GL.MatrixComponent a, GL.UniformComponent a) => GL.UniformLocation -> M44 a -> IO () 
m44Setter loc mat = do
    let l = foldr (\i a -> toList i <> a) [] mat
    mat <- GL.newMatrix GL.RowMajor l :: IO (GL.GLmatrix a)
    GL.uniform loc $= (mat :: GL.GLmatrix a)

toM44 :: [a] -> M44 a
toM44 mat = V4 (toV4 row1) (toV4 row2) (toV4 row3) (toV4 row4)
    where 
        (row1, rest1) = splitAt 4 mat
        (row2, rest2) = splitAt 4 rest1
        (row3, rest3) = splitAt 4 rest2
        (row4, []) = splitAt 4 rest3

toV4 :: [a] -> V4 a 
toV4 [x, y, z, w] = V4 x y z w