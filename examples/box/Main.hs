
module Main where

import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL
import qualified Data.Matrix as M
import Linear
import Graphics.Rendering.OpenGL (($=))
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Data.Time (getCurrentTime, diffUTCTime, NominalDiffTime)
import Shader
import Text.RawString.QQ
import Data.Foldable

windowSize = (1920, 1080)
windowTitle = "Triangle example"

errorCallback err msg = do
    putStrLn "GLFW Error Occurred: "
    print err 
    putStrLn $ "with message: " ++ msg

debugCallback msg = do
    putStr "GL Error Occurred: "
    print msg

data GameState = GameState 
    { shader :: GL.Program
    , cube :: GL.VertexArrayObject
    }

vert = [r|
#version 330 core
layout (location = 0) in vec3 position;
layout (location = 1) in vec3 normal;

out vec3 FragPos;
out vec3 Normal;

uniform mat4 projection;
uniform mat4 view; 
// uniform vec2 screenSize;
uniform mat4 model;

void main()
{
    FragPos = vec3(model * vec4(position, 1.0));
    Normal = mat3(transpose(inverse(model))) * normal;

    gl_Position = projection * view * vec4(FragPos, 1.0);
}
|]

frag = [r|
#version 330 core

in vec3 FragPos;
in vec3 Normal;

out vec4 FragColor;

uniform vec4 ambientColor;
uniform vec4 objectColor;

uniform vec3 viewPos;

uniform vec3 lightPos;
uniform vec4 lightColor;

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
    vec4 result = (ambient + diffuse + specular) * objectColor;
    FragColor = result;
}
|]

createCube :: IO GL.VertexArrayObject
createCube = do
    
    let verticesL = [ (-0.5), (-0.5), (-0.5), ( 0.0), ( 0.0), (-1.0)
                    , ( 0.5), (-0.5), (-0.5), ( 0.0), ( 0.0), (-1.0)
                    , ( 0.5), ( 0.5), (-0.5), ( 0.0), ( 0.0), (-1.0)
                    , ( 0.5), ( 0.5), (-0.5), ( 0.0), ( 0.0), (-1.0)
                    , (-0.5), ( 0.5), (-0.5), ( 0.0), ( 0.0), (-1.0)
                    , (-0.5), (-0.5), (-0.5), ( 0.0), ( 0.0), (-1.0)

                    , (-0.5), (-0.5), ( 0.5), ( 0.0), ( 0.0), ( 1.0)
                    , ( 0.5), (-0.5), ( 0.5), ( 0.0), ( 0.0), ( 1.0)
                    , ( 0.5), ( 0.5), ( 0.5), ( 0.0), ( 0.0), ( 1.0)
                    , ( 0.5), ( 0.5), ( 0.5), ( 0.0), ( 0.0), ( 1.0)
                    , (-0.5), ( 0.5), ( 0.5), ( 0.0), ( 0.0), ( 1.0)
                    , (-0.5), (-0.5), ( 0.5), ( 0.0), ( 0.0), ( 1.0)

                    , (-0.5), ( 0.5), ( 0.5), (-1.0), ( 0.0), ( 0.0)
                    , (-0.5), ( 0.5), (-0.5), (-1.0), ( 0.0), ( 0.0)
                    , (-0.5), (-0.5), (-0.5), (-1.0), ( 0.0), ( 0.0)
                    , (-0.5), (-0.5), (-0.5), (-1.0), ( 0.0), ( 0.0)
                    , (-0.5), (-0.5), ( 0.5), (-1.0), ( 0.0), ( 0.0)
                    , (-0.5), ( 0.5), ( 0.5), (-1.0), ( 0.0), ( 0.0)

                    , ( 0.5), ( 0.5), ( 0.5), ( 1.0), ( 0.0), ( 0.0)
                    , ( 0.5), ( 0.5), (-0.5), ( 1.0), ( 0.0), ( 0.0)
                    , ( 0.5), (-0.5), (-0.5), ( 1.0), ( 0.0), ( 0.0)
                    , ( 0.5), (-0.5), (-0.5), ( 1.0), ( 0.0), ( 0.0)
                    , ( 0.5), (-0.5), ( 0.5), ( 1.0), ( 0.0), ( 0.0)
                    , ( 0.5), ( 0.5), ( 0.5), ( 1.0), ( 0.0), ( 0.0)

                    , (-0.5), (-0.5), (-0.5), ( 0.0), (-1.0), ( 0.0)
                    , ( 0.5), (-0.5), (-0.5), ( 0.0), (-1.0), ( 0.0)
                    , ( 0.5), (-0.5), ( 0.5), ( 0.0), (-1.0), ( 0.0)
                    , ( 0.5), (-0.5), ( 0.5), ( 0.0), (-1.0), ( 0.0)
                    , (-0.5), (-0.5), ( 0.5), ( 0.0), (-1.0), ( 0.0)
                    , (-0.5), (-0.5), (-0.5), ( 0.0), (-1.0), ( 0.0)

                    , (-0.5), ( 0.5), (-0.5), ( 0.0), ( 1.0), ( 0.0)
                    , ( 0.5), ( 0.5), (-0.5), ( 0.0), ( 1.0), ( 0.0)
                    , ( 0.5), ( 0.5), ( 0.5), ( 0.0), ( 1.0), ( 0.0)
                    , ( 0.5), ( 0.5), ( 0.5), ( 0.0), ( 1.0), ( 0.0)
                    , (-0.5), ( 0.5), ( 0.5), ( 0.0), ( 1.0), ( 0.0)
                    , (-0.5), ( 0.5), (-0.5), ( 0.0), ( 1.0), ( 0.0)
                    ] :: [Float]
    vertices <- newArray verticesL
    let verticesSize = fromIntegral $ sizeOf (0.0 :: Float) * length verticesL

    vao <- GL.genObjectName :: IO GL.VertexArrayObject
    vbo <- GL.genObjectName :: IO GL.BufferObject

    GL.bindVertexArrayObject $= Just vao

    GL.bindBuffer GL.ArrayBuffer $= Just vbo
    GL.bufferData GL.ArrayBuffer $= (verticesSize, vertices, GL.StaticDraw)

    GL.vertexAttribPointer (GL.AttribLocation 0) $= (GL.ToFloat, GL.VertexArrayDescriptor 3 GL.Float (fromIntegral $ 6 * sizeOf (0.0 :: Float)) nullPtr)
    GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Enabled

    GL.vertexAttribPointer (GL.AttribLocation 1) $= (GL.ToFloat, GL.VertexArrayDescriptor 3 GL.Float (fromIntegral $ 6 * sizeOf (0.0 :: Float)) (nullPtr `plusPtr` (3 * (sizeOf (0.0 :: Float)))))
    GL.vertexAttribArray (GL.AttribLocation 1) $= GL.Enabled

    GL.bindBuffer GL.ArrayBuffer $= Nothing
    GL.bindVertexArrayObject $= Nothing
    GL.deleteObjectName vbo

    return vao


main :: IO ()
main = do
    GLFW.setErrorCallback $ Just errorCallback

    initialized <- GLFW.init
    if not initialized
        then errorCallback GLFW.Error'NotInitialized "Game Initialization Failed"
        else do

            GLFW.windowHint (GLFW.WindowHint'ContextVersionMajor 3)
            GLFW.windowHint (GLFW.WindowHint'ContextVersionMinor 3)
            GLFW.windowHint (GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core)
            GLFW.windowHint (GLFW.WindowHint'Resizable False)
        
            let (width, height) = windowSize
        
            mwindow <- GLFW.createWindow width height windowTitle Nothing Nothing
        
            GLFW.makeContextCurrent mwindow
        
            window <- case mwindow of
                Nothing -> error "window failed to initialize"
                Just window -> do
                    
                    GL.debugOutput $= GL.Enabled
                    GL.debugMessageCallback $= Just debugCallback
        
                    (width, height) <- GLFW.getFramebufferSize window
                    GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral width) (fromIntegral height))
        
                    GL.blend $= GL.Enabled
                    GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
                    GL.depthFunc $= Just GL.Lequal
        
                    return window



            Just program <- compileShader vert frag Nothing 

            GL.currentProgram $= Just program

            let fov = 45 :: Float
            let ratio = 1920 / 1080.0
            let near = 0.1
            let far = 100

            projection <- perspectiveMatrix fov ratio near far

            projectionLoc <- GL.get $ GL.uniformLocation program "projection"
            GL.uniform projectionLoc $= projection

            GL.currentProgram $= Nothing

            cube <- createCube

            startTime <- getCurrentTime

            state <- mainLoop window startTime (GameState program cube)

            GLFW.terminate


mainLoop window previousTime (GameState program cube) = do
    GLFW.pollEvents

    GL.clearColor $= GL.Color4 0 0 0 1
    GL.clear [GL.ColorBuffer, GL.DepthBuffer]

    thisTime <- getCurrentTime
    let deltaTime = diffUTCTime thisTime previousTime
    let dt = (realToFrac deltaTime :: Double)

    -- render
    GL.currentProgram $= Just program

    model <- toGLMatrix $ M.fromList 4 4
        [ 1.0, 0.0, 0.0, 0.0
        , 0.0, 1.0, 0.0, 0.0
        , 0.0, 0.0, 1.0, 0.0
        , 0.0, 0.0, 0.0, 1.0 :: Float
        ]

    modelLoc <- GL.get $ GL.uniformLocation program "model"
    objectColorLoc <- GL.get $ GL.uniformLocation program "objectColor"
    ambientColorLoc <- GL.get $ GL.uniformLocation program "ambientColor"
    viewLoc <- GL.get $ GL.uniformLocation program "view"

    viewPosLoc <- GL.get $ GL.uniformLocation program "viewPos"
    
    lightColorLoc <- GL.get $ GL.uniformLocation program "lightColor"
    lightPosLoc <- GL.get $ GL.uniformLocation program "lightPos"

    GL.uniform modelLoc $= model
    GL.uniform objectColorLoc $= (GL.Color4 1.0 0.5 0.3 1.0 :: GL.Color4 Float)
    GL.uniform ambientColorLoc $= (GL.Color4 1.0 1.0 1.0 1.0 :: GL.Color4 Float)

    GL.uniform viewPosLoc $= (GL.Vector3 1 1 1 :: GL.Vector3 Float)

    GL.uniform lightPosLoc $= (GL.Vector3 1.1 1.0 2.0 :: GL.Vector3 Float)
    GL.uniform lightColorLoc $= (GL.Color4 1.0 1.0 1.0 1.0 :: GL.Color4 Float)

    view <- lookAtMatrix (V3 1 1 1) (V3 0 0 0) (V3 0 1 0) :: IO (GL.GLmatrix Float)

    GL.uniform viewLoc $= view

    GL.bindVertexArrayObject $= Just cube
    GL.drawArrays GL.Triangles 0 36 -- size of the cube array
    GL.bindVertexArrayObject $= Nothing

    GLFW.swapBuffers window
    shouldClose <- GLFW.windowShouldClose window
    if shouldClose 
        then return ()
        else mainLoop window thisTime (GameState program cube)


-- | Create an OpenGL matrix from the Matrix
toGLMatrix  :: GL.MatrixComponent a
            => M.Matrix a  -- ^ Input Matrix
            -> IO (GL.GLmatrix a) -- ^ Output GLmatrix
toGLMatrix mat = GL.newMatrix GL.RowMajor (M.toList mat)

perspectiveMatrix :: (Floating a, GL.MatrixComponent a) => a -> a -> a -> a -> IO (GL.GLmatrix a)
perspectiveMatrix fov ratio near far = do
    let persMat = perspective fov ratio near far
    let persL = foldr (\i a -> toList i <> a) [] persMat
    GL.newMatrix GL.RowMajor $ persL

lookAtMatrix :: (Epsilon a, Floating a, GL.MatrixComponent a) => V3 a -> V3 a -> V3 a -> IO (GL.GLmatrix a)
lookAtMatrix eye center up = do
    let persMat = lookAt eye center up
    let persL = foldr (\i a -> toList i <> a) [] persMat
    GL.newMatrix GL.RowMajor $ persL