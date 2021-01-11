
module Main where

import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import Data.Time (getCurrentTime, diffUTCTime, NominalDiffTime)
import Shader
import Text.RawString.QQ

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
    }

vert = [r|
#version 330 core
layout (location = 0) in vec4 position;

uniform mat4 projection;
// uniform vec2 screenSize;
uniform mat4 model;

void main()
{
    gl_Position = projection * model * vec4(position.xy, 0.0, 1.0);
}
|]

frag = [r|
#version 330 core
out vec4 color;

uniform vec4 spriteColor;

void main()
{
    color = spriteColor;
}
|]


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

                    Just program <- compileShader vert frag Nothing 
        
        
                    return window

            startTime <- getCurrentTime

            state <- mainLoop window startTime

            GLFW.terminate


mainLoop window previousTime = do
    GLFW.pollEvents

    GL.clearColor $= GL.Color4 0 0 0 1
    GL.clear [GL.ColorBuffer, GL.DepthBuffer]

    thisTime <- getCurrentTime
    let deltaTime = diffUTCTime thisTime previousTime
    let dt = (realToFrac deltaTime :: Double)

    -- render

    GLFW.swapBuffers window
    shouldClose <- GLFW.windowShouldClose window
    if shouldClose 
        then return ()
        else mainLoop window thisTime