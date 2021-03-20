module Engine.Vector where


import qualified Data.Matrix as M
import Graphics.Rendering.OpenGL hiding (lookAt, perspective, position)
import qualified Linear as L
import Data.Foldable
import Engine.Types
import Control.Lens

-- | Create an OpenGL matrix from the Matrix
toGLMatrix  :: MatrixComponent a
            => M.Matrix a  -- ^ Input Matrix
            -> IO (GLmatrix a) -- ^ Output GLmatrix
toGLMatrix mat = newMatrix RowMajor (M.toList mat)


-- | Creates a 4x4 transform matrix representing a translation 
translationMat :: Fractional a => Vector3 a -> M.Matrix a
translationMat (Vector3 x y z) = mat
    where mat = M.fromList 4 4 [ 1.0, 0.0, 0.0, x 
                               , 0.0, 1.0, 0.0, y
                               , 0.0, 0.0, 1.0, z
                               , 0.0, 0.0, 0.0, 1.0
                               ]

translationMatrix :: (MatrixComponent a, Fractional a) => Vector3 a -> IO (GLmatrix a)
translationMatrix = toGLMatrix . translationMat 


-- | Creates a 4x4 transform matrix representing a scaling 
scaleMat :: Fractional a => Vector3 a -> M.Matrix a
scaleMat (Vector3 width height depth) = mat
    where mat = M.fromList 4 4 [ width,  0.0, 0.0, 0.0 
                               , 0.0, height, 0.0, 0.0 
                               , 0.0, 0.0, depth,  0.0 
                               , 0.0, 0.0,    0.0, 1.0
                               ]

scaleMatrix :: (MatrixComponent a, Fractional a) => Vector3 a -> IO (GLmatrix a)
scaleMatrix = toGLMatrix . scaleMat

perspectiveMatrix :: (Floating a, MatrixComponent a) => a -> a -> a -> a -> IO (GLmatrix a)
perspectiveMatrix fov ratio near far = do
    let persMat = L.perspective fov ratio near far
    let persL = foldr (\i a -> toList i <> a) [] persMat
    newMatrix RowMajor persL

lookAtMatrix :: (L.Epsilon a, Floating a, MatrixComponent a) => Vector3 a -> Vector3 a -> Vector3 a -> IO (GLmatrix a)
lookAtMatrix (Vector3 ex ey ez) (Vector3 cx cy cz) (Vector3 ux uy uz) = do
    let persMat = L.lookAt (L.V3 ex ey ez) (L.V3 cx cy cz) (L.V3 ux uy uz)
    let persL = foldr (\i a -> toList i <> a) [] persMat
    newMatrix RowMajor persL


eye4 :: (Fractional a, MatrixComponent a) => IO (GLmatrix a)
eye4 = toGLMatrix $ M.fromList 4 4
        [ 1.0, 0.0, 0.0, 0.0
        , 0.0, 1.0, 0.0, 0.0
        , 0.0, 0.0, 1.0, 0.0
        , 0.0, 0.0, 0.0, 1.0
        ]

        
rotationMatrix :: L.V3 Float -> L.M33 Float
rotationMatrix (L.V3 x y z) = L.fromQuaternion $ qx * qy * qz
    where
        qx = L.axisAngle (L.V3 1 0 0) (degToRad x)
        qy = L.axisAngle (L.V3 0 1 0) (degToRad y)
        qz = L.axisAngle (L.V3 0 0 1) (degToRad z)
        degToRad = (*pi).(/180)

-- -- | Creates a 4x4 transform matrix representing a rotation 
-- rotationMat2D   :: Float -- ^ Rotation in rads
--                 -> M.Matrix Float
-- rotationMat2D rot = mat
--     where mat = M.fromList 4 4 [ cos rot, -(sin rot), 0.0, 0.0 
--                                , sin rot, cos rot,    0.0, 0.0 
--                                , 0.0, 0.0,            1.0, 0.0 
--                                , 0.0, 0.0,            0.0, 1.0
--                                ]


-- -- | Creats an orthogrphic projection matrix
-- orthographic :: Float -> Float -> Float -> Float -> Float -> Float -> M.Matrix Float
-- orthographic left right bottom top near far = mat
--     where mat = M.fromList 4 4 [ 2.0 / (right - left), 0.0, 0.0, - (right + left) / (right - left) 
--                                , 0.0, 2.0 / (top - bottom), 0.0, - (top + bottom) / (top - bottom)
--                                , 0.0, 0.0, - 2.0 / (far - near), - (far + near) / (far - near)
--                                , 0.0, 0.0, 0.0, 1.0
--                                ]



-- -- | Creates the sprites VAO and adds the goemetry
-- createSquareVAO :: IO VertexArrayObject
-- createSquareVAO = do
    
--     let verticesL = [ (-0.5), (-0.5),   0.0, 0.0
--                     , 0.5, 0.5,         1.0, 1.0
--                     , (-0.5), 0.5,      0.0, 1.0
                    
--                     , (-0.5), (-0.5),   0.0, 0.0
--                     , 0.5, (-0.5),      1.0, 0.0
--                     , 0.5, 0.5,         1.0, 1.0
--                     ] :: [Float]
--     vertices <- newArray verticesL
--     let verticesSize = fromIntegral $ sizeOf (0.0 :: Float) * length verticesL

--     vao <- genObjectName :: IO VertexArrayObject
--     vbo <- genObjectName :: IO BufferObject

--     bindVertexArrayObject $= Just vao

--     bindBuffer ArrayBuffer $= Just vbo
--     bufferData ArrayBuffer $= (verticesSize, vertices, StaticDraw)

--     vertexAttribPointer (AttribLocation 0) $= (ToFloat, VertexArrayDescriptor 4 Float (fromIntegral $ 4 * sizeOf (0.0 :: Float)) nullPtr)
--     vertexAttribArray (AttribLocation 0) $= Enabled

--     bindBuffer ArrayBuffer $= Nothing
--     bindVertexArrayObject $= Nothing
--     deleteObjectName vbo

--     return vao

-- | Create a 4x4 transformation matrix from Transform object
-- getTransformMatrix :: Transform -> M.Matrix Float
-- getTransformMatrix (Transform pos size rot) = model
--     where 
--         translate = translationMat pos
--         rotate = rotationMat2D (pi / 180 * rot)
--         scale = scaleMat size
--         model = translate * rotate * scale