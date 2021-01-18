module Window where

import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import Control.Monad.IO.Class

errorCallback err msg = do
    putStrLn "GLFW Error Occurred: "
    print err 
    putStrLn $ "with message: " ++ msg

debugCallback msg = do
    putStr "GL Error Occurred: "
    print msg

initWindow :: MonadIO m => GL.Size -> String -> m GLFW.Window
initWindow size@(GL.Size width height) title = liftIO do
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
        
                    (width, height) <- GLFW.getFramebufferSize window
                    GL.viewport $= (GL.Position 0 0, size)
        
                    GL.blend $= GL.Enabled
                    GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
                    GL.depthFunc $= Just GL.Lequal
        
                    return window
            
