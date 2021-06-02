module Engine.Window where

import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import Control.Monad.IO.Class
import Linear
import Engine.Types
import Engine.Events
import Control.Monad
import Data.IORef
import Data.Time (getCurrentTime, diffUTCTime)

errorCallback :: Show a => a -> [Char] -> IO ()
errorCallback err msg = do
    putStrLn "GLFW Error Occurred: "
    print err 
    putStrLn $ "with message: " ++ msg

debugCallback :: Show a => a -> IO ()
debugCallback msg = do
    print msg

initWindow :: MonadIO m => V2 Int -> String -> m GLFW.Window
initWindow size title = do
    initWindowWithErrors size title errorCallback debugCallback

initWindowWithErrors :: MonadIO m => V2 Int -> String -> GLFW.ErrorCallback -> (GL.DebugMessage -> IO ()) -> m GLFW.Window
initWindowWithErrors (V2 width height) title errorCallback debugCallback = liftIO do
    GLFW.setErrorCallback $ Just errorCallback

    initialized <- GLFW.init
    if not initialized
        then do
            errorCallback GLFW.Error'NotInitialized "Game Initialization Failed"
            error "Game Initialization Failed"
        else do

            GLFW.windowHint (GLFW.WindowHint'ContextVersionMajor 3)
            GLFW.windowHint (GLFW.WindowHint'ContextVersionMinor 3)
            GLFW.windowHint (GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core)
            GLFW.windowHint (GLFW.WindowHint'Resizable False)
                
            mwindow <- GLFW.createWindow (fromIntegral width) (fromIntegral height) title Nothing Nothing
        
            GLFW.makeContextCurrent mwindow
        
            case mwindow of
                Nothing -> error "window failed to initialize"
                Just window -> do
                    
                    GL.debugOutput $= GL.Enabled
                    GL.debugMessageCallback $= Just debugCallback
        
                    GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral width) (fromIntegral height))
        
                    GL.blend $= GL.Enabled
                    GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
                    GL.depthFunc $= Just GL.Lequal
        
                    return window
playIO :: IORef w
       -> GLFW.Window
       -> (w -> IO ())
       -> (Event -> w -> IO w)
       -> (Double -> w -> IO w)
       -> IO ()
playIO worldRef win draw handle step = do
    let debug = True
    
    initEventCallbacks win handle worldRef

    prevTime <- getCurrentTime
    let loop prevTime = do
            GLFW.pollEvents

            GL.clearColor $= GL.Color4 0 0 0 1
            GL.clear [GL.ColorBuffer, GL.DepthBuffer]


            newTime <- getCurrentTime
            let delta = realToFrac $ diffUTCTime newTime prevTime
            world' <- readIORef worldRef
            world'' <- step delta world'
            draw world''
            writeIORef worldRef world''

            GLFW.swapBuffers win
            shouldClose <- GLFW.windowShouldClose win
            unless shouldClose $ loop newTime
    loop prevTime
