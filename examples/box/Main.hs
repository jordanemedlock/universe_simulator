
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
import Linear


data GameState = GameState 
    { shader :: GL.Program
    , cube :: GL.VertexArrayObject
    }

vert :: ByteString
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

frag :: ByteString
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


    -- must be vbo then vao
    vao <- tempVBO GL.ArrayBuffer $ initVAO $ liftIO do
        GL.bufferData GL.ArrayBuffer $= (verticesSize, vertices, GL.StaticDraw)

        GL.vertexAttribPointer (GL.AttribLocation 0) $= (GL.ToFloat, GL.VertexArrayDescriptor 3 GL.Float (fromIntegral $ 6 * sizeOf (0.0 :: Float)) nullPtr)
        GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Enabled

        GL.vertexAttribPointer (GL.AttribLocation 1) $= (GL.ToFloat, GL.VertexArrayDescriptor 3 GL.Float (fromIntegral $ 6 * sizeOf (0.0 :: Float)) (nullPtr `plusPtr` (3 * (sizeOf (0.0 :: Float)))))
        GL.vertexAttribArray (GL.AttribLocation 1) $= GL.Enabled
    return vao


main :: IO ()
main = do
    window <- initWindow (V2 1920 1080) "Box example"

    Just program <- compileShader vert frag Nothing 

    let fov = 45 :: Float
    let ratio = 1920 / 1080.0
    let near = 0.1
    let far = 100

    projection <- perspectiveMatrix fov ratio near far

    withShader program $ "projection" $== projection

    cubeVAO <- createCube

    startTime <- getCurrentTime

    mainLoop window startTime (GameState program cubeVAO)

    GLFW.terminate

mainLoop :: GLFW.Window -> UTCTime -> GameState -> IO ()
mainLoop window previousTime (GameState program cubeVAO) = do
    GLFW.pollEvents

    GL.clearColor $= GL.Color4 0 0 0 1
    GL.clear [GL.ColorBuffer, GL.DepthBuffer]

    thisTime <- getCurrentTime
    let deltaTime = diffUTCTime thisTime previousTime
    let dt = (realToFrac deltaTime :: Double)

    model <- eye4 :: IO (GL.GLmatrix Float)
    
    view <- lookAtMatrix (GL.Vector3 1 1 1) (GL.Vector3 0 0 0) (GL.Vector3 0 1 0) :: IO (GL.GLmatrix Float)

    withShader program do
        "model"         $== model
        "view"          $== view

        "objectColor"   $== (GL.Color4 1.0 0.5 0.3 1.0 :: GL.Color4 Float)
        "ambientColor"  $== (GL.Color4 1.0 1.0 1.0 1.0 :: GL.Color4 Float)
        "viewPos"       $== (GL.Vector3 1 1 1 :: GL.Vector3 Float)
        "lightPos"      $== (GL.Vector3 1.1 1.0 2.0 :: GL.Vector3 Float)
        "lightColor"    $== (GL.Color4 1.0 1.0 1.0 1.0 :: GL.Color4 Float)

        withVAO cubeVAO $ liftIO $ GL.drawArrays GL.Triangles 0 36 -- size of the cube array
        

    GLFW.swapBuffers window
    shouldClose <- GLFW.windowShouldClose window
    if shouldClose 
        then return ()
        else mainLoop window thisTime (GameState program cubeVAO)

