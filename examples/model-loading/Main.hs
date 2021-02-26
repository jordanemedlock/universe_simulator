{-# LANGUAGE DuplicateRecordFields, ScopedTypeVariables #-}
module Main where

import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Data.Time (getCurrentTime, diffUTCTime, UTCTime)
import Engine
import Text.RawString.QQ
import Control.Monad.IO.Class
import qualified Data.ByteString as BS
import GHC.Word
import Types
import Control.Lens hiding (transform)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import Linear hiding (vector)
import Data.IORef
import qualified Codec.GlTF as GLTF
import qualified Codec.GlTF.Image as Image
import qualified Codec.GlTF.URI as URI
import qualified Codec.GlTF.Mesh as Mesh
import qualified Codec.GlTF.Buffer as Buffer
import qualified Codec.GlTF.Accessor as Accessor
import qualified Codec.GlTF.Material as Material
import Data.Either
import qualified Data.Vector as V
import qualified Data.Text as T
import System.FilePath.Posix
import qualified Data.HashMap.Strict as HM
import qualified Codec.GlTF.BufferView as BufferView
import Data.String (IsString)



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

createSphere = do

    let n = 20

    let verticesL = fullSphereVertexList n
    vertices <- newArray verticesL
    let verticesSize = fromIntegral $ sizeOf (0.0 :: Float) * length verticesL

    let indicesL = fromIntegral <$> sphereIndexList n :: [Word32]
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
    GL.bufferData GL.ElementArrayBuffer $= (indicesSize, indices, GL.StaticDraw)
    GL.bindBuffer GL.ElementArrayBuffer $= Just indVBO

    GL.bindVertexArrayObject $= Nothing
    GL.deleteObjectName vertVBO
    GL.deleteObjectName indVBO
    return (vao, indicesSize `div` 3)

data GLTFAsset = GLTFAsset { textures :: V.Vector GL.TextureObject
                           , objects :: V.Vector GL.VertexArrayObject
                           } deriving Show

loadGLTFAsset :: FilePath -> IO (Either String GLTFAsset)
loadGLTFAsset file = runExceptT do
    model <- ExceptT $ GLTF.fromFile file

    textures <- ExceptT $ loadTextures model $ takeDirectory file
    objs <- ExceptT $ loadObjects model $ takeDirectory file

    return $ GLTFAsset textures objs

maybeToRight :: b -> Maybe a -> Either b a
maybeToRight b (Just a) = Right a
maybeToRight b Nothing = Left b

fromMaybe :: (Monad m) => Maybe a -> ExceptT String m a
fromMaybe x = ExceptT $ return $ maybeToRight "value doesnt exist" x

forM = flip mapM

loadBuffer :: Buffer.Buffer -> String -> IO (Either String BS.ByteString)
loadBuffer buffer root = runExceptT do
    URI.URI fileName <- fromMaybe $ Buffer.uri buffer
    liftIO $ BS.readFile $ root </> T.unpack fileName

loadBufferView :: BufferView.BufferView -> BS.ByteString -> Either String BS.ByteString
loadBufferView bufferView buffer = runExcept do
    let offset = BufferView.byteOffset bufferView
    let length = BufferView.byteLength bufferView
    let (_start, rest) = BS.splitAt offset buffer
    let (newBuffer, _rest) = BS.splitAt length rest
    return newBuffer

data AccessorValue = AccessorValue {} deriving Show

loadAccessors :: GLTF.GlTF -> String -> IO (Either String (V.Vector BS.ByteString))
loadAccessors model root = runExceptT do
    buffers <- fromMaybe $ GLTF.buffers model
    bufferViews <- fromMaybe $ GLTF.bufferViews model
    accessors <- fromMaybe $ GLTF.accessors model

    bufferByteStrings <- liftIO $ mapM (`loadBuffer` root) buffers

    forM accessors $ \acc -> do
        bvIdx <- fromMaybe $ (Accessor.bufferView :: Accessor.Accessor -> Maybe BufferView.BufferViewIx) acc
        bufferView <- V.indexM bufferViews $ BufferView.unBufferViewIx bvIdx
        buffer <- ExceptT $ V.indexM bufferByteStrings $ Buffer.unBufferIx $ BufferView.buffer bufferView
        ExceptT $ return $ loadBufferView bufferView buffer

gltfNameToAttribName "TEXCOORD_0" = "texCoord0"
gltfNameToAttribName name = T.toLower name

loadObjects :: GLTF.GlTF -> String -> IO (Either String (V.Vector GL.VertexArrayObject))
loadObjects model root = runExceptT do
    meshObjs <- fromMaybe $ GLTF.meshes model
    buffers <- fromMaybe $ GLTF.buffers model
    bufferViews <- fromMaybe $ GLTF.bufferViews model
    accessors <- fromMaybe $ GLTF.accessors model

    buffersBS <- liftIO $ V.fromList.rights.V.toList <$> mapM (`loadBuffer` root) buffers


    meshes <- forM meshObjs $ \mesh -> forM (Mesh.primitives mesh) \primitive -> do
        indicesIx <- Accessor.unAccessorIx <$> fromMaybe (Mesh.indices primitive)
        indicesAcc <- fromMaybe $ V.indexM accessors indicesIx
        indicesBVI <- fromMaybe $ (Accessor.bufferView :: Accessor.Accessor -> Maybe BufferView.BufferViewIx) indicesAcc
        indicesBV <- fromMaybe $ V.indexM bufferViews $ BufferView.unBufferViewIx indicesBVI
        indicesBS <- fromMaybe $ V.indexM buffersBS $ Buffer.unBufferIx $ BufferView.buffer indicesBV
        -- TODO: assuming vertices and indices are on same buffer :-/
        materialIx <- fromMaybe $ Mesh.material primitive
        -- materialBS <- V.indexM accessorsBS $ Material.unMaterialIx materialIx
        let mode = Mesh.mode primitive
        let attributes = Mesh.attributes primitive
        liftIO do
            vao <- GL.genObjectName
            -- TODO: pull all data (index and vertex) into the same buffer object.
            -- you will need to find how to specify the index on the buffer.
            -- actually I can keep indices on a separate buffer and keep all the attributes on
            -- the same buffer.  I can also iterate over each of them and give them all 
            -- attribute locations based on their name.  so "NORMAL" => attribLocation "normal"
            indicesVBO <- GL.genObjectName :: IO GL.BufferObject
            vertexVBO <- GL.genObjectName :: IO GL.BufferObject

            GL.bindVertexArrayObject $= Just vao

            GL.bindBuffer GL.ElementArrayBuffer $= Just indicesVBO
            BS.useAsCString indicesBS $ \ptr -> do
                -- TODO this ignores stride length, this could be bad lol
                GL.bufferData GL.ElementArrayBuffer $= (fromIntegral $ BufferView.byteLength indicesBV, ptr `plusPtr` BufferView.byteOffset indicesBV, GL.StaticDraw) 
            GL.bindBuffer GL.ElementArrayBuffer $= Nothing

            GL.bindBuffer GL.ArrayBuffer $= Just vertexVBO
            BS.useAsCString indicesBS $ \ptr -> do
                GL.bufferData GL.ArrayBuffer $= (fromIntegral $ BS.length indicesBS, ptr, GL.StaticDraw)

            let inner = \(name, accessorIx) -> do
                let attribName = gltfNameToAttribName name
                -- TODO: appearantly this needs program, but it feels weird to supply it just for this
                loc <- liftIO $ GL.get $ GL.attribLocation program (T.unpack attribName)
                accessor <- fromMaybe $ V.indexM (Accessor.unAccessorIx accessorIx)

                count <- fromMaybe $ Accessor.count accessor
                bufferViewIx <- fromMaybe $ (Accessor.bufferView :: Accessor.Accessor -> Maybe BufferView.BufferViewIx) accessor

                bufferView <- fromMaybe $ V.indexM  
                GL.vertexAttribPointer loc $= (GL.ToFloat, GL.VertexArrayDescriptor 3 GL.Float (fromIntegral $ BS.length positionBS))


            x <- forM (HM.toList attributes) inner

            liftIO $ error "stop here"


            liftIO $ print vao
            return vao

    liftIO $ print meshes
    return $ V.concat $ V.toList meshes

loadTextures :: GLTF.GlTF -> String -> IO (Either String (V.Vector GL.TextureObject))
loadTextures model root = runExceptT do
    texObjs <- fromMaybe $ GLTF.textures model
    imgObjs <- fromMaybe $ GLTF.images model
    textures <- forM imgObjs $ \img -> do
        URI.URI uri <- fromMaybe $ Image.uri img
        let path = root </> T.unpack uri
        (obj, _size) <- liftIO $ loadTexture path
        return obj

    liftIO $ print texObjs
    return textures

initAssets :: IO Assets
initAssets = do
    vert <- BS.readFile "resources/shaders/full.vert"
    frag <- BS.readFile "resources/shaders/full.frag"
    Just program <- compileShader vert frag Nothing


    asset <- loadGLTFAsset "resources/AnimatedCube/AnimatedCube.gltf"
    print asset

    error "some shit"

    (sphereVAO, numTris) <- createSphere

    (tex, _) <- loadTexture "resources/8k_earth_daymap.png"

    return $ Assets program (EarthAssets sphereVAO numTris tex)


initState :: IO GameState
initState = do
    startTime <- getCurrentTime

    let cameraState = Camera 0 0 (V3 2 2 2) 70 (16.0/9.0) 0.1 100.0

    let earthState = Earth $ Transform (EulerRotation $ V3 (-90) 0 0) $ V3 0 0 0

    return $ GameState cameraState earthState startTime Nothing Playing

projection :: Camera -> M44 Float
projection cam = perspective (cam^.fov) (cam^.ratio) (cam^.near) (cam^.far)

mkRotationMat :: Rotation Float -> M33 Float
mkRotationMat (QuaternionRotation quat) = fromQuaternion quat
mkRotationMat (EulerRotation (V3 x y z)) = fromQuaternion $ qx * qy * qz
    where
        qx = axisAngle (V3 1 0 0) (degToRad x)
        qy = axisAngle (V3 0 1 0) (degToRad y)
        qz = axisAngle (V3 0 0 1) (degToRad z)

transformationMatrix :: Transform Float -> M44 Float
transformationMatrix trans = mkTransformationMat (mkRotationMat $ trans^.rotation) $ trans^.position

cameraMatrix :: Camera -> M44 Float
cameraMatrix cam = lookAt (cam^.position) (cam^.position + cameraForward cam) (V3 0 1 0)

cameraForward :: Camera -> V3 Float
cameraForward cam = normalize $ V3 dx dy dz
    where
        dx = cos (degToRad $ cam^.yaw) * cos (degToRad $ cam^.pitch)
        dy = sin (degToRad $ cam^.pitch)
        dz = sin (degToRad $ cam^.yaw) * cos (degToRad $ cam^.pitch)

cameraRight :: Camera -> V3 Float
cameraRight cam = normalize $ cross (cameraForward cam) (V3 0 1 0)

cameraUp :: Camera -> V3 Float
cameraUp cam = normalize $ cross (cameraRight cam) (cameraForward cam)

moveCameraForward :: Float -> Camera -> Camera
moveCameraForward speed cam = cam & position +~ cameraForward cam ^* speed

moveCameraRight :: Float -> Camera -> Camera
moveCameraRight speed cam = cam & position +~ cameraRight cam ^* speed

moveCameraUp :: Float -> Camera -> Camera
moveCameraUp speed cam = cam & position +~ cameraUp cam ^* speed

degToRad :: Float -> Float
degToRad = (*pi).(/180)

main :: IO ()
main = do
    window <- initWindow (V2 800 600) "Box example"

    -- GL.polygonMode $= (GL.Line, GL.Line)

    let fov = 45 :: Float
    let ratio = 800 / 600.0
    let near = 0.1
    let far = 100

    projection <- perspectiveMatrix fov ratio near far

    assets <- initAssets
    state <- newIORef =<< initState

    let game = Game window assets state

    GLFW.setCursorInputMode window GLFW.CursorInputMode'Disabled
    GLFW.setKeyCallback window $ Just $ keyCallback game
    GLFW.setCursorPosCallback window $ Just $ cursorPosCallback game


    runReaderT mainLoop game

    GLFW.terminate

keyCallback :: Game -> GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys -> IO ()
keyCallback game win GLFW.Key'Escape i keyState modifiers = modifyIORef (game^.gameState) $ mode .~ Stopped

keyCallback game win GLFW.Key'W i keyState modifiers = modifyIORef (game^.gameState) $ camera %~ moveCameraForward (0.01)
keyCallback game win GLFW.Key'A i keyState modifiers = modifyIORef (game^.gameState) $ camera %~ moveCameraRight (-0.01)
keyCallback game win GLFW.Key'S i keyState modifiers = modifyIORef (game^.gameState) $ camera %~ moveCameraForward (-0.01)
keyCallback game win GLFW.Key'D i keyState modifiers = modifyIORef (game^.gameState) $ camera %~ moveCameraRight (0.01)
keyCallback game win GLFW.Key'Q i keyState modifiers = modifyIORef (game^.gameState) $ camera %~ moveCameraUp (0.01)
keyCallback game win GLFW.Key'E i keyState modifiers = modifyIORef (game^.gameState) $ camera %~ moveCameraUp (-0.01)

keyCallback game win key i keyState modifiers = do
    return ()

clamp :: (Ord a) => a -> a -> a -> a
clamp min max x | x < min = min
                | x > max = max
                | otherwise = x

cursorPosCallback :: Game -> GLFW.Window -> Double -> Double -> IO ()
cursorPosCallback game win x y = do
    lastPos <- maybe (V2 x y) id . view lastCursorPos <$> readIORef (game^.gameState)
    -- print lastPos
    let cameraSensitivity = 0.1
    let dx = realToFrac $ (x - (lastPos^._x)) * cameraSensitivity
    let dy = realToFrac $ ((lastPos^._y) - y) * cameraSensitivity
    -- putStrLn $ show (V2 x y) <> " - " <> show lastPos <> " => " <> show (V2 dx dy)
    modifyIORef (game^.gameState) $ ((camera.pitch) %~ (clamp (-89) 89).(+dy))
                                  . ((camera.yaw) +~ dx)
                                  . (lastCursorPos .~ Just (V2 x y))

mainLoop :: ReaderT Game IO ()
mainLoop = do
    game <- ask
    state <- liftIO $ readIORef $ game^.gameState
    case state^.mode of
        Playing -> playing
        Paused -> playing
        Stopped -> return ()

playing :: ReaderT Game IO ()
playing = do
    game <- ask
    state <- liftIO $ readIORef $ game^.gameState
    let earthA = game^.assets^.earth
    let earthT = state^.earth^.transform
    let cam = state^.camera

    thisTime <- liftIO do
        GLFW.pollEvents

        GL.clearColor $= GL.Color4 0 0 0 1
        GL.clear [GL.ColorBuffer, GL.DepthBuffer]

        getCurrentTime
    let deltaTime = diffUTCTime thisTime (state^.lastTime)
    let dt = (realToFrac deltaTime :: Double)
    let speed = 0

    liftIO do
        modifyIORef (game^.gameState) $ (earth.transform.rotation.euler._z) %~ (+(realToFrac $ dt*speed))
        modifyIORef (game^.gameState) $ (lastTime) .~ thisTime

    -- liftIO $ putStrLn $ "yaw: " ++ (show $ cam^.yaw) ++ " pitch: " ++ (show $ cam^.pitch)

    let view = cameraMatrix cam


    let model = transformationMatrix earthT
    -- let view = mkTransformationMat (mkRotationMat $ cameraT^.rotation) $ cameraT^.position
    -- let pos = V3 (2) (2) (2) :: V3 Float

    -- let view = lookAt pos (V3 0 0 0) (V3 0 1 0) :: M44 Float

    withShader (game^.assets^.shader) do
        "model"         $== model -- TODO: add a uniform instance for Tranform
        "view"          $== view
        "projection"    $== projection cam

        "objectColor"   $== (color 1.0 0.5 0.3 1.0)
        "ambientColor"  $== (color 1.0 1.0 1.0 1.0)
        "viewPos"       $== (cam^.position)
        "lightPos"      $== (V3 1.1 1.0 2.0 :: V3 Float)
        "lightColor"    $== (color 1.0 1.0 1.0 1.0)


        liftIO $ GL.activeTexture $= GL.TextureUnit 0
        liftIO $ GL.textureBinding GL.Texture2D $= Just (earthA^.texture)

        withVAO (earthA^.object) $ liftIO $ GL.drawElements GL.Triangles (fromIntegral (earthA^.numTris)) GL.UnsignedInt nullPtr -- size of the cube array


    shouldClose <- liftIO do

        GLFW.swapBuffers (game^.window)
        GLFW.windowShouldClose (game^.window)
    if shouldClose
        then return ()
        else mainLoop

