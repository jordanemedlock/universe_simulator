
module Main where

import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import Engine
import Linear

windowSize :: V2 Int
windowSize = V2 1920 1080

windowTitle :: String
windowTitle = "Triangle example"

main :: IO ()
main = do
    window <- initWindow windowSize windowTitle
    mainLoop window

    GLFW.terminate

mainLoop :: GLFW.Window -> IO ()
mainLoop window = do
    GLFW.pollEvents

    GL.clearColor $= GL.Color4 0 0 0 1
    GL.clear [GL.ColorBuffer, GL.DepthBuffer]

    -- render

    GLFW.swapBuffers window
    shouldClose <- GLFW.windowShouldClose window
    if shouldClose 
        then return ()
        else mainLoop window