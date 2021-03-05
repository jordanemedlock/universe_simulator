
module Main where

import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Data.Time (getCurrentTime, diffUTCTime, UTCTime)
import Engine
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import GHC.Word
import Types
import Control.Lens hiding (transform)
import Control.Monad.Trans.Reader
import Linear hiding (vector)
import Data.IORef
import Data.Maybe
import Control.Monad




initAssets :: IO Assets
initAssets = do
    Right program <- loadShader "resources/shaders/lighting"

    sphereMesh <- createSphereAsset 20

    (tex, _) <- loadTexture "resources/textures/8k_earth_daymap.png"

    return $ Assets program (EarthAssets sphereMesh tex)


initState :: IO GameState
initState = do
    startTime <- getCurrentTime

    let cameraState = Camera 0 0 (V3 2 2 2) 70 (16.0/9.0) 0.1 100.0

    let earthState = Earth $ Transform (EulerRotation $ V3 (-90) 0 0) $ V3 0 0 0

    return $ GameState cameraState earthState startTime Nothing Playing

main :: IO ()
main = do
    window <- initWindow (V2 1920 1080) "Camera movement example"

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
    lastPos <- fromMaybe (V2 x y) . view lastCursorPos <$> readIORef (game^.gameState)
    let cameraSensitivity = 0.1
    let dx = realToFrac $ (x - (lastPos^._x)) * cameraSensitivity
    let dy = realToFrac $ ((lastPos^._y) - y) * cameraSensitivity
    modifyIORef (game^.gameState) $ ((camera.pitch) %~ clamp (-89) 89.(+dy))
                                  . ((camera.yaw) +~ dx)
                                  . (lastCursorPos ?~ V2 x y)

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
    let earthA = game^.assets.earth
    let earthT = state^.earth.transform
    let cam = state^.camera

    thisTime <- liftIO do
        GLFW.pollEvents

        GL.clearColor $= GL.Color4 0 0 0 1
        GL.clear [GL.ColorBuffer, GL.DepthBuffer]

        getCurrentTime
    let deltaTime = diffUTCTime thisTime (state^.lastTime)
    let dt = (realToFrac deltaTime :: Double)
    let speed = 1

    liftIO do
        modifyIORef (game^.gameState) $ ((earth.transform.rotation.euler._z) %~ (+(realToFrac $ dt*speed)))
                                      . (lastTime .~ thisTime)

    let view = cameraMatrix cam

    let model = transformationMatrix earthT

    withShader (game^.assets.shader) do
        "model"         $== model
        "view"          $== view
        "projection"    $== projection cam

        "objectColor"   $== color 1.0 0.5 0.3 1.0
        "ambientColor"  $== color 1.0 1.0 1.0 1.0
        "viewPos"       $== (cam^.position)
        "lightPos"      $== (V3 1.1 1.0 2.0 :: V3 Float)
        "lightColor"    $== color 1.0 1.0 1.0 1.0


        liftIO $ GL.activeTexture $= GL.TextureUnit 0
        liftIO $ GL.textureBinding GL.Texture2D $= Just (earthA^.texture)

        drawMeshAsset (earthA^.mesh) -- size of the cube array


    shouldClose <- liftIO do
        GLFW.swapBuffers (game^.window)
        GLFW.windowShouldClose (game^.window)
    unless shouldClose mainLoop

