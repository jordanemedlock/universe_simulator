{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FunctionalDependencies #-}
module Engine.Mesh where

import qualified Graphics.Rendering.OpenGL as GL
import Control.Lens.TH
import Graphics.Rendering.OpenGL (($=))
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Control.Monad.IO.Class
import Engine.Shader
import GHC.Int
import GHC.Word
import Engine.Types
import Linear

createCubeAsset :: MonadIO m => m MeshAsset
createCubeAsset = liftIO do
    
    let verticesL = [ (-0.5), (-0.5), (-0.5),    ( 0.0), ( 0.0), (-1.0),    ( 0.0), ( 0.0)
                    , ( 0.5), (-0.5), (-0.5),    ( 0.0), ( 0.0), (-1.0),    ( 1.0), ( 0.0)
                    , ( 0.5), ( 0.5), (-0.5),    ( 0.0), ( 0.0), (-1.0),    ( 1.0), ( 1.0)
                    , ( 0.5), ( 0.5), (-0.5),    ( 0.0), ( 0.0), (-1.0),    ( 1.0), ( 1.0)
                    , (-0.5), ( 0.5), (-0.5),    ( 0.0), ( 0.0), (-1.0),    ( 0.0), ( 1.0)
                    , (-0.5), (-0.5), (-0.5),    ( 0.0), ( 0.0), (-1.0),    ( 0.0), ( 0.0)

                    , (-0.5), (-0.5), ( 0.5),    ( 0.0), ( 0.0), ( 1.0),    ( 0.0), ( 0.0)
                    , ( 0.5), (-0.5), ( 0.5),    ( 0.0), ( 0.0), ( 1.0),    ( 1.0), ( 0.0)
                    , ( 0.5), ( 0.5), ( 0.5),    ( 0.0), ( 0.0), ( 1.0),    ( 1.0), ( 1.0)
                    , ( 0.5), ( 0.5), ( 0.5),    ( 0.0), ( 0.0), ( 1.0),    ( 1.0), ( 1.0)
                    , (-0.5), ( 0.5), ( 0.5),    ( 0.0), ( 0.0), ( 1.0),    ( 0.0), ( 1.0)
                    , (-0.5), (-0.5), ( 0.5),    ( 0.0), ( 0.0), ( 1.0),    ( 0.0), ( 1.0)

                    , (-0.5), ( 0.5), ( 0.5),    (-1.0), ( 0.0), ( 0.0),    ( 1.0), ( 1.0)
                    , (-0.5), ( 0.5), (-0.5),    (-1.0), ( 0.0), ( 0.0),    ( 1.0), ( 0.0)
                    , (-0.5), (-0.5), (-0.5),    (-1.0), ( 0.0), ( 0.0),    ( 0.0), ( 0.0)
                    , (-0.5), (-0.5), (-0.5),    (-1.0), ( 0.0), ( 0.0),    ( 0.0), ( 0.0)
                    , (-0.5), (-0.5), ( 0.5),    (-1.0), ( 0.0), ( 0.0),    ( 0.0), ( 1.0)
                    , (-0.5), ( 0.5), ( 0.5),    (-1.0), ( 0.0), ( 0.0),    ( 1.0), ( 1.0)

                    , ( 0.5), ( 0.5), ( 0.5),    ( 1.0), ( 0.0), ( 0.0),    ( 1.0), ( 1.0)
                    , ( 0.5), ( 0.5), (-0.5),    ( 1.0), ( 0.0), ( 0.0),    ( 1.0), ( 0.0)
                    , ( 0.5), (-0.5), (-0.5),    ( 1.0), ( 0.0), ( 0.0),    ( 0.0), ( 0.0)
                    , ( 0.5), (-0.5), (-0.5),    ( 1.0), ( 0.0), ( 0.0),    ( 0.0), ( 0.0)
                    , ( 0.5), (-0.5), ( 0.5),    ( 1.0), ( 0.0), ( 0.0),    ( 0.0), ( 1.0)
                    , ( 0.5), ( 0.5), ( 0.5),    ( 1.0), ( 0.0), ( 0.0),    ( 1.0), ( 1.0)

                    , (-0.5), (-0.5), (-0.5),    ( 0.0), (-1.0), ( 0.0),    ( 0.0), ( 0.0)
                    , ( 0.5), (-0.5), (-0.5),    ( 0.0), (-1.0), ( 0.0),    ( 1.0), ( 0.0)
                    , ( 0.5), (-0.5), ( 0.5),    ( 0.0), (-1.0), ( 0.0),    ( 1.0), ( 1.0)
                    , ( 0.5), (-0.5), ( 0.5),    ( 0.0), (-1.0), ( 0.0),    ( 1.0), ( 1.0)
                    , (-0.5), (-0.5), ( 0.5),    ( 0.0), (-1.0), ( 0.0),    ( 0.0), ( 1.0)
                    , (-0.5), (-0.5), (-0.5),    ( 0.0), (-1.0), ( 0.0),    ( 0.0), ( 0.0)

                    , (-0.5), ( 0.5), (-0.5),    ( 0.0), ( 1.0), ( 0.0),    ( 0.0), ( 0.0)
                    , ( 0.5), ( 0.5), (-0.5),    ( 0.0), ( 1.0), ( 0.0),    ( 1.0), ( 0.0)
                    , ( 0.5), ( 0.5), ( 0.5),    ( 0.0), ( 1.0), ( 0.0),    ( 1.0), ( 1.0)
                    , ( 0.5), ( 0.5), ( 0.5),    ( 0.0), ( 1.0), ( 0.0),    ( 1.0), ( 1.0)
                    , (-0.5), ( 0.5), ( 0.5),    ( 0.0), ( 1.0), ( 0.0),    ( 0.0), ( 1.0)
                    , (-0.5), ( 0.5), (-0.5),    ( 0.0), ( 1.0), ( 0.0),    ( 0.0), ( 0.0)
                    ] :: [Float]
    vertices <- newArray verticesL
    let verticesSize = fromIntegral $ sizeOf (0.0 :: Float) * length verticesL


    -- must be vbo then vao
    vao <- tempVBO GL.ArrayBuffer $ initVAO $ liftIO do
        GL.bufferData GL.ArrayBuffer $= (verticesSize, vertices, GL.StaticDraw)

        GL.vertexAttribPointer (GL.AttribLocation 0) $= (GL.ToFloat, GL.VertexArrayDescriptor 3 GL.Float (fromIntegral $ 8 * sizeOf (0.0 :: Float)) nullPtr)
        GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Enabled

        GL.vertexAttribPointer (GL.AttribLocation 1) $= (GL.ToFloat, GL.VertexArrayDescriptor 3 GL.Float (fromIntegral $ 8 * sizeOf (0.0 :: Float)) (nullPtr `plusPtr` (3 * (sizeOf (0.0 :: Float)))))
        GL.vertexAttribArray (GL.AttribLocation 1) $= GL.Enabled

        GL.vertexAttribPointer (GL.AttribLocation 2) $= (GL.ToFloat, GL.VertexArrayDescriptor 2 GL.Float (fromIntegral $ 8 * sizeOf (0.0 :: Float)) (nullPtr `plusPtr` (6 * (sizeOf (0.0 :: Float)))))
        GL.vertexAttribArray (GL.AttribLocation 2) $= GL.Enabled
    return $ MeshAsset vao 36 Arrays GL.Triangles GL.UnsignedInt

drawMeshAsset :: MonadIO m => MeshAsset -> m ()
drawMeshAsset (MeshAsset vao numTris Arrays mode dataType) = liftIO do
    withVAO vao $ liftIO $ GL.drawArrays mode 0 numTris
drawMeshAsset (MeshAsset vao numTris Elements mode dataType) = liftIO do
    withVAO vao $ liftIO $ GL.drawElements mode numTris dataType nullPtr

planeVertexList :: Int -> Int -> [(Float, Float, Float)]
planeVertexList rows cols = concat [ [ (fromIntegral r / fromIntegral (rows-1), fromIntegral c / fromIntegral (cols-1), 0.0) 
                                     | c <- [0..cols-1]
                                     ] 
                                   | r <- [0..rows-1]
                                   ]

planeIndexList :: Int -> Int -> [Int]
planeIndexList rows cols = concat $ concat  [ [ (if r == 0           then [] else [ c + r * cols,       c + r * cols + 1,       c + (r + 1) * cols ])
                                                ++
                                                (if r == (rows-2)    then [] else [ c + r * cols + 1,   c + (r + 1) * cols + 1, c + (r + 1) * cols ])
                                                | c <- [0..cols-2]
                                                ]
                                            | r <- [0..rows-2]
                                            ]

fullPlaneVertexList :: Int -> Int -> [Float]
fullPlaneVertexList rows cols = concat [ [ x - 0.5, y - 0.5, z, 0, 0, 1.0, y, x ]
                                       | (x, y, z) <- planeVertexList rows cols
                                       ]

sphereVertexList :: Int -> [(Float,Float,Float)] -- list of rows of vertices
sphereVertexList n = (\(x, y, z) -> (sin (pi*x) * cos (2*pi*y), sin (pi*x) * sin (2*pi*y), cos (pi*x))) <$> planeVertexList n (n*2)


sphereIndexList :: Int -> [Int]
sphereIndexList n = planeIndexList n (n*2)

fullSphereVertexList :: Int -> [Float]
fullSphereVertexList n = concat [ [ x, y, z, x, y, z, u, v ] 
                                | (i, (x, y, z)) <- zip [0..] $ sphereVertexList n
                                , let (iv, iu) = i `divMod` (n*2)
                                , let fn = fromIntegral n
                                , let (u, v) = (fromIntegral iu / fn / 2, fromIntegral iv / fn)
                                ]
    
newPlaneIndexList :: Int -> Int -> [Int]
newPlaneIndexList rows cols = concat $ concat [ [ [ c + r * cols,      c + r * cols + 1,       c + (r + 1) * cols
                                                  , c + r * cols + 1,  c + (r + 1) * cols + 1, c + (r + 1) * cols
                                                  ]
                                                  | c <- [0..cols-2]
                                                ] 
                                                | r <- [0..rows-2]
                                              ]

createMeshAsset :: (MonadIO m) => [Float] -> [Int] -> m MeshAsset
createMeshAsset verts inds = liftIO do
    vertices <- newArray verts
    let verticesSize = fromIntegral $ sizeOf (0.0 :: Float) * length verts

    let indicesL = fromIntegral <$> inds :: [Word32]
    indices <- newArray indicesL
    let indicesSize = fromIntegral $ sizeOf (0 :: Word32) * length indicesL

    vao <- GL.genObjectName :: IO GL.VertexArrayObject
    vertVBO <- GL.genObjectName :: IO GL.BufferObject
    indVBO <- GL.genObjectName :: IO GL.BufferObject

    GL.bindVertexArrayObject $= Just vao

    GL.bindBuffer GL.ArrayBuffer $= Just vertVBO
    GL.bufferData GL.ArrayBuffer $= (verticesSize, vertices, GL.StaticDraw)

    GL.vertexAttribPointer (GL.AttribLocation 0) $= (GL.ToFloat, GL.VertexArrayDescriptor 3 GL.Float (fromIntegral $ 8 * sizeOf (0.0 :: Float)) nullPtr)
    GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Enabled

    GL.vertexAttribPointer (GL.AttribLocation 1) $= (GL.ToFloat, GL.VertexArrayDescriptor 3 GL.Float (fromIntegral $ 8 * sizeOf (0.0 :: Float)) (nullPtr `plusPtr` (3 * (sizeOf (0.0 :: Float)))))
    GL.vertexAttribArray (GL.AttribLocation 1) $= GL.Enabled

    GL.vertexAttribPointer (GL.AttribLocation 2) $= (GL.ToFloat, GL.VertexArrayDescriptor 2 GL.Float (fromIntegral $ 8 * sizeOf (0.0 :: Float)) (nullPtr `plusPtr` (6 * (sizeOf (0.0 :: Float)))))
    GL.vertexAttribArray (GL.AttribLocation 2) $= GL.Enabled

    GL.bindBuffer GL.ElementArrayBuffer $= Just indVBO
    GL.bufferData GL.ElementArrayBuffer $= (fromIntegral indicesSize, indices, GL.StaticDraw)
    GL.bindBuffer GL.ElementArrayBuffer $= Just indVBO

    GL.bindVertexArrayObject $= Nothing
    GL.deleteObjectName vertVBO
    GL.deleteObjectName indVBO
    return $ MeshAsset vao (indicesSize `div` 3) Elements GL.Triangles GL.UnsignedInt


createSphereAsset :: MonadIO m => Int -> m MeshAsset
createSphereAsset n = liftIO do
    createMeshAsset (fullSphereVertexList n) (sphereIndexList n)

createPlaneAsset :: MonadIO m => Int -> m MeshAsset
createPlaneAsset n = liftIO do
    createMeshAsset (fullPlaneVertexList n n) (newPlaneIndexList n n)

createLinesAsset :: MonadIO m => [V3 Float] -> m MeshAsset
createLinesAsset points = liftIO do
    let verts = concat [[x,y,z,0,0,1,0,0] | V3 x y z <- points]
    let indices = concat [ [i, i, i+1]
                         | i <- [0..length points-2]
                         ]
    createMeshAsset verts indices
