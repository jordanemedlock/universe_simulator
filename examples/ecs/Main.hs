
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
import Data.ByteString (ByteString)
import GHC.Word
import Types
import Control.Lens hiding (transform)
import Control.Monad.Trans.Reader
import Linear hiding (vector)
import Data.IORef




vert :: ByteString
vert = [r|
#version 330 core
layout (location = 0) in vec3 position;
layout (location = 1) in vec3 normal;
layout (location = 2) in vec2 texCoords;

out vec3 FragPos;
out vec3 Normal;
out vec2 TexCoord;

uniform mat4 projection;
uniform mat4 view; 
// uniform vec2 screenSize;
uniform mat4 model;

void main()
{
    FragPos = vec3(model * vec4(position, 1.0));
    Normal = mat3(transpose(inverse(model))) * normal;

    gl_Position = projection * view * vec4(FragPos, 1.0);
    TexCoord = texCoords;
}
|]

frag :: ByteString
frag = [r|
#version 330 core

in vec3 FragPos;
in vec3 Normal;
in vec2 TexCoord;

out vec4 FragColor;

uniform vec4 ambientColor;
uniform vec4 objectColor;

uniform vec3 viewPos;

uniform vec3 lightPos;
uniform vec4 lightColor;

uniform sampler2D objectTexture;

void main()
{
    // constants
    float ambientStrength = 0.1;
    float specularStrength = 0.5;

    // diffuse
    vec3 norm = normalize(Normal);
    vec3 lightDir = normalize(lightPos - FragPos);  
    float diff = max(dot(norm, lightDir), 0.0);
    vec4 diffuse = diff * lightColor;

    // ambient
    vec4 ambient = ambientStrength * ambientColor;

    // specular
    vec3 viewDir = normalize(viewPos - FragPos);
    vec3 reflectDir = reflect(-lightDir, norm);
    float spec = pow(max(dot(viewDir, reflectDir), 0.0), 32);
    vec4 specular = specularStrength * spec * lightColor;

    // result
    // vec4 result = (ambient + diffuse + specular) * texture(objectTexture, TexCoord);
    vec4 result = texture(objectTexture, TexCoord);
    FragColor = result;
}
|]

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
fullPlaneVertexList rows cols = concat [ [ x, y, z, 0, 0, 1.0, y, x ]
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

createCube :: IO GL.VertexArrayObject
createCube = do
    
    let verticesL = [ (-0.5), (-0.5), (-0.5),   ( 0.0), ( 0.0), (-1.0),     0.0, 0.0
                    , ( 0.5), (-0.5), (-0.5),   ( 0.0), ( 0.0), (-1.0),     1.0, 0.0
                    , ( 0.5), ( 0.5), (-0.5),   ( 0.0), ( 0.0), (-1.0),     1.0, 1.0
                    , ( 0.5), ( 0.5), (-0.5),   ( 0.0), ( 0.0), (-1.0),     1.0, 1.0
                    , (-0.5), ( 0.5), (-0.5),   ( 0.0), ( 0.0), (-1.0),     0.0, 1.0
                    , (-0.5), (-0.5), (-0.5),   ( 0.0), ( 0.0), (-1.0),     0.0, 0.0

                    , (-0.5), (-0.5), ( 0.5),   ( 0.0), ( 0.0), ( 1.0),     0.0, 0.0
                    , ( 0.5), (-0.5), ( 0.5),   ( 0.0), ( 0.0), ( 1.0),     1.0, 0.0
                    , ( 0.5), ( 0.5), ( 0.5),   ( 0.0), ( 0.0), ( 1.0),     1.0, 1.0
                    , ( 0.5), ( 0.5), ( 0.5),   ( 0.0), ( 0.0), ( 1.0),     1.0, 1.0
                    , (-0.5), ( 0.5), ( 0.5),   ( 0.0), ( 0.0), ( 1.0),     0.0, 1.0
                    , (-0.5), (-0.5), ( 0.5),   ( 0.0), ( 0.0), ( 1.0),     0.0, 0.0

                    , (-0.5), ( 0.5), ( 0.5),   (-1.0), ( 0.0), ( 0.0),     1.0, 1.0
                    , (-0.5), ( 0.5), (-0.5),   (-1.0), ( 0.0), ( 0.0),     1.0, 0.0
                    , (-0.5), (-0.5), (-0.5),   (-1.0), ( 0.0), ( 0.0),     0.0, 0.0
                    , (-0.5), (-0.5), (-0.5),   (-1.0), ( 0.0), ( 0.0),     0.0, 0.0
                    , (-0.5), (-0.5), ( 0.5),   (-1.0), ( 0.0), ( 0.0),     0.0, 1.0
                    , (-0.5), ( 0.5), ( 0.5),   (-1.0), ( 0.0), ( 0.0),     1.0, 1.0

                    , ( 0.5), ( 0.5), ( 0.5),   ( 1.0), ( 0.0), ( 0.0),     1.0, 1.0
                    , ( 0.5), ( 0.5), (-0.5),   ( 1.0), ( 0.0), ( 0.0),     1.0, 0.0
                    , ( 0.5), (-0.5), (-0.5),   ( 1.0), ( 0.0), ( 0.0),     0.0, 0.0
                    , ( 0.5), (-0.5), (-0.5),   ( 1.0), ( 0.0), ( 0.0),     0.0, 0.0
                    , ( 0.5), (-0.5), ( 0.5),   ( 1.0), ( 0.0), ( 0.0),     0.0, 1.0
                    , ( 0.5), ( 0.5), ( 0.5),   ( 1.0), ( 0.0), ( 0.0),     1.0, 1.0

                    , (-0.5), (-0.5), (-0.5),   ( 0.0), (-1.0), ( 0.0),     0.0, 0.0
                    , ( 0.5), (-0.5), (-0.5),   ( 0.0), (-1.0), ( 0.0),     1.0, 0.0
                    , ( 0.5), (-0.5), ( 0.5),   ( 0.0), (-1.0), ( 0.0),     1.0, 1.0
                    , ( 0.5), (-0.5), ( 0.5),   ( 0.0), (-1.0), ( 0.0),     1.0, 1.0
                    , (-0.5), (-0.5), ( 0.5),   ( 0.0), (-1.0), ( 0.0),     0.0, 1.0
                    , (-0.5), (-0.5), (-0.5),   ( 0.0), (-1.0), ( 0.0),     0.0, 0.0

                    , (-0.5), ( 0.5), (-0.5),   ( 0.0), ( 1.0), ( 0.0),     0.0, 0.0
                    , ( 0.5), ( 0.5), (-0.5),   ( 0.0), ( 1.0), ( 0.0),     1.0, 0.0
                    , ( 0.5), ( 0.5), ( 0.5),   ( 0.0), ( 1.0), ( 0.0),     1.0, 1.0
                    , ( 0.5), ( 0.5), ( 0.5),   ( 0.0), ( 1.0), ( 0.0),     1.0, 1.0
                    , (-0.5), ( 0.5), ( 0.5),   ( 0.0), ( 1.0), ( 0.0),     0.0, 1.0
                    , (-0.5), ( 0.5), (-0.5),   ( 0.0), ( 1.0), ( 0.0),     0.0, 0.0
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
    return vao

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

initAssets :: IO Assets
initAssets = do
    Just program <- compileShader vert frag Nothing 


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
    window <- initWindow (V2 1920 1080) "Box example"

    -- GL.polygonMode $= (GL.Line, GL.Line)
    
    let fov = 45 :: Float
    let ratio = 1920 / 1080.0
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

