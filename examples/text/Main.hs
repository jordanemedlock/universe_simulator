{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleContexts      #-}
module Main where

import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import Data.Time (getCurrentTime, diffUTCTime)
import Engine hiding (Camera)
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Linear
import Data.IORef
import Control.Monad
import Apecs hiding (($=), asks, ask)
import qualified Apecs
import Apecs.TH
import System.Exit
import Control.Lens
import Data.Maybe (fromMaybe)


data Event = KeyEvent GLFW.Key Int GLFW.KeyState GLFW.ModifierKeys
           | CursorEvent Double Double
           deriving (Show)


newtype Pos = Pos (V3 Float) deriving Show
newtype Rot = Rot (Either (V3 Float) (Quaternion Float)) deriving Show
newtype Scale = Scale (V3 Float) deriving Show
newtype Size2D = Size2D (V2 Float) deriving Show
data CamRot = CamRot Float Float deriving Show

newtype Texture = Texture GL.TextureObject deriving Show
newtype Shader = Shader GL.Program deriving Show
newtype Mesh = Mesh MeshAsset deriving Show

data Orbit = Orbit { rotation :: Float
                   , revolution :: Float
                   , rotationSpeed :: Float 
                   , revolutionSpeed :: Float 
                   , center :: V3 Float
                   , radius :: Float
                   } deriving Show

data TextBox = TextBox { strings :: [String]
                       , textFont :: Entity
                       , textColor :: V4 Float
                       , textSize :: Float
                       } deriving Show

data Earth = Earth deriving Show
instance Component Earth where type Storage Earth = Unique Earth

data Moon = Moon deriving Show
instance Component Moon where type Storage Moon = Unique Moon

data Camera = Camera deriving Show
instance Component Camera where type Storage Camera = Unique Camera

data GameState = Paused | Playing | Closed deriving (Show, Eq, Enum)
instance Semigroup GameState where (<>) = const
instance Monoid GameState where mempty = Playing
instance Component GameState where type Storage GameState = Global GameState

data CursorPos = CursorPos Double Double deriving (Show, Eq)
instance Semigroup CursorPos where (CursorPos x1 y1) <> (CursorPos x2 y2) = CursorPos (x1+x2) (y1+y2)
instance Monoid CursorPos where mempty = CursorPos 0 0
instance Component CursorPos where type Storage CursorPos = Global CursorPos

data GameSettings = GameSettings { cameraSpeed :: (Float, Float) -- (cursor, keyboard)
                                 } deriving Show
instance Semigroup GameSettings where (<>) = const
instance Monoid GameSettings where mempty = GameSettings (0.1, 0.01)
instance Component GameSettings where type Storage GameSettings = Global GameSettings

makeMapComponents [ ''Pos, ''Rot, ''Texture, ''Shader, ''Orbit, ''Mesh, ''CamRot, ''Scale, ''Font, ''TextBox ]
makeWorld "World" [ ''Pos, ''Rot, ''Texture, ''Shader, ''CamRot, ''Scale, ''Font
                  , ''Orbit, ''Mesh, ''Earth, ''Moon, ''TextBox
                  , ''Camera, ''GameState, ''GameSettings, ''CursorPos
                  ]

main :: IO ()
main = do
    let debug = True
    when debug $ putStrLn "Initiating world"
    world <- initWorld

    when debug $ putStrLn "Initiating window"
    window <- initWindow (V2 1920 1080) "Camera movement example"

    -- GL.polygonMode $= (GL.Line, GL.Line)

    runWith world $ do

        liftIO $ when debug $ putStrLn "Initiating systems"
        initialize

        liftIO $ when debug $ putStrLn "playing game"
        play window draw handle step

    GLFW.terminate

play :: GLFW.Window
     -> System w ()
     -> (Event -> System w ())
     -> (Double -> System w ())
     -> System w ()
play win draw handle step = do
    w <- Apecs.ask
    let draw' = runSystem draw
    let handle' event = runSystem $ handle event >> Apecs.ask
    let step' delta = runSystem $ step delta >> Apecs.ask
    liftIO $ playIO w win draw' handle' step'

playIO :: w
       -> GLFW.Window
       -> (w -> IO ())
       -> (Event -> w -> IO w)
       -> (Double -> w -> IO w)
       -> IO ()
playIO world win draw handle step = do
    let debug = True

    worldRef <- newIORef world


    GLFW.setCursorInputMode win GLFW.CursorInputMode'Disabled
    GLFW.setKeyCallback win $ Just $ \w k i ks ms -> do
        world' <- readIORef worldRef
        world'' <- handle (KeyEvent k i ks ms) world'
        writeIORef worldRef world''
    GLFW.setCursorPosCallback win $ Just $ \w x y -> do
        world' <- readIORef worldRef
        world'' <- handle (CursorEvent x y) world'
        writeIORef worldRef world''

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

initialize :: System World ()
initialize = do
    (earthTex, _) <- loadTexture "resources/textures/8k_earth_daymap.png"
    (moonTex, _) <- loadTexture "resources/textures/8k_moon.png"
    program <- either error id <$> loadShader "resources/shaders/lighting"
    glyph <- either error id <$> loadShader "resources/shaders/glyph"
    font <- loadFont "resources/fonts/Inconsolata.ttf" 48 glyph

    

    withShader glyph do
        "projection" $== ortho 0 1920 1080 0 (-1) (1 :: Float)


    sphereMesh <- createSphereAsset 20

    cameraEty <- newEntity (Camera, Pos $ V3 2 2 2, CamRot 0 0)
    earthEty <- newEntity ( Earth
                        , Pos $ V3 0 0 0, Rot $ Left $ V3 (-90) 0 0, Scale $ V3 1 1 1
                        , Texture earthTex, Shader program, Mesh sphereMesh
                        , Orbit 0 0 0.1 0 (V3 0 0 0) 0
                        )
    moonEty <- newEntity  ( Moon
                        , Pos $ V3 10 0 0, Rot $ Left $ V3 0 0 0, Scale $ V3 0.1 0.1 0.1
                        , Texture moonTex, Shader program, Mesh sphereMesh
                        , Orbit 0 0 0.1 0.1 (V3 0 0 0) 10
                        )
    fontEty <- newEntity font
    hudEty <- newEntity ( TextBox ["Hello World!"] fontEty (V4 1 1 1 1) 0.5, Pos $ V3 10 30 0 )

    return ()

draw = drawObjects >> drawText

drawObjects :: System World ()
drawObjects = cmapM_ $ \(Camera, Pos camPos, CamRot pitch yaw) -> do
    cmapM_ $ \(Texture tex, Shader sdr, Mesh mesh, Pos pos, Rot erot, mscale) -> do
        let view = lookAt camPos (camPos + cameraForward pitch yaw) (V3 0 1 0)
        let proj = perspective (70 :: Float) (16.0/9) 0.1 100.0
        let (Scale size) = fromMaybe (Scale $ V3 1 1 1) mscale
        let scaleMat = scaled $ point size
        let rotMat = either rotationMatrix fromQuaternion erot
        let model = mkTransformationMat rotMat pos !*! scaleMat


        withShader sdr do
            "model"         $== model
            "view"          $== view
            "projection"    $== proj

            "objectColor"   $== color 1.0 0.5 0.3 1.0
            "ambientColor"  $== color 1.0 1.0 1.0 1.0
            "viewPos"       $== camPos
            "lightPos"      $== (V3 1.1 1.0 2.0 :: V3 Float)
            "lightColor"    $== color 1.0 1.0 1.0 1.0

            liftIO $ GL.activeTexture $= GL.TextureUnit 0
            liftIO $ GL.textureBinding GL.Texture2D $= Just tex

            drawMeshAsset mesh -- size of the cube array

drawText :: System World ()
drawText = cmapM_ \(TextBox strings fontEty color size, Pos pos) -> do
    font <- get fontEty

    forM_ (zip [0..] strings) $ \(row, str) -> do
        -- liftIO $ putStrLn $ "Drawing string " <> show str
        renderString font str (pos^._xy + V2 0 (realToFrac row * 24)) size color
    

moveCamera :: (MonadIO m) => (Float -> Float -> V3 Float) -> Float -> SystemT World m ()
moveCamera dir vel = cmap \(Camera, Pos pos, CamRot pitch yaw, GameSettings {cameraSpeed=(_, speed)}) ->
    (Pos $ pos + dir pitch yaw ^* vel ^* speed)

handle :: (MonadIO m) => Event -> SystemT World m ()
handle (KeyEvent GLFW.Key'Escape i ks mods) = liftIO exitSuccess
handle (KeyEvent GLFW.Key'W i ks mods) = moveCamera cameraForward (1)
handle (KeyEvent GLFW.Key'A i ks mods) = moveCamera cameraRight (-1)
handle (KeyEvent GLFW.Key'S i ks mods) = moveCamera cameraForward (-1)
handle (KeyEvent GLFW.Key'D i ks mods) = moveCamera cameraRight (1)
handle (KeyEvent GLFW.Key'Q i ks mods) = moveCamera cameraUp (1)
handle (KeyEvent GLFW.Key'E i ks mods) = moveCamera cameraUp (-1)

handle (CursorEvent x y) = do
    (CursorPos px py, GameSettings {cameraSpeed=(speed, _)}) <- get global

    let dx = realToFrac (x - px) * speed
    let dy = realToFrac (py - y) * speed
    cmap \(Camera, CamRot pitch yaw) -> (Camera, CamRot (clamp (-89) 89 (pitch + dy)) (yaw + dx), CursorPos x y)

handle _ = do
    return ()

runOrbit :: Double -> System World ()
runOrbit delta = cmap inner
    where 
        inner (Orbit rot rev rotS revS c r, Pos _, Rot _) = ( Orbit rot' rev' rotS revS c r, Pos pos', Rot (Right rotM))
            where
                rot' = rot + rotS * realToFrac delta
                rev' = rev + revS * realToFrac delta
                x = r * sin rev'
                z = r * cos rev'
                pos' = V3 x 0 z
                rotM = axisAngle (V3 0 1 0) rot'

step :: Double -> System World ()
step delta = do
    runOrbit delta

clamp :: (Ord a) => a -> a -> a -> a
clamp min max x | x < min = min
                | x > max = max
                | otherwise = x
