{-# LANGUAGE TemplateHaskell,NamedFieldPuns,PatternSynonyms,DuplicateRecordFields,TupleSections #-}

module Main where

import Engine
import Engine.Apecs
import Types
import Apecs hiding (($=))
import Apecs.TH
import Apecs.Components
import Apecs.Core (ExplMembers(..))
import Linear
import Control.Monad
import Data.Vector ((!), (!?))
import Graphics.UI.GLFW (Key(..), KeyState(..))
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import System.Exit
import Text.Printf
import Data.Maybe
import qualified Data.ByteString as BS
import Control.Monad.IO.Class
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import System.FilePath.Posix
import Foreign.Ptr

instance Component GLFW.Window where type Storage GLFW.Window = Unique GLFW.Window


makeMapComponents $ defaultMapComponents <> mapComponents
makeWorld "World" $ defaultAllComponents <> mapComponents <> nonMapComponents <> [''GLFW.Window]

-- type RandIO = RandT StdGen IO
type WorldSystem a = SystemT World IO a -- I want dem monads!!!!

getRight :: Either a b -> b
getRight (Right x) = x

getJust :: Maybe a -> a
getJust (Just a) = a


main = do
    world <- initWorld

    runWith world $ do

        settings <- get global

        debugLog "Initiating window"
        window <- initWindow (screenSize settings) "Solar System"

        liftIO $ GLFW.setCursorInputMode window GLFW.CursorInputMode'Disabled

        _ <- newEntity window

        debugLog "Initiating system"
        initialize

        debugLog "Playing game"
        play window draw handle step

debugLog msg = do
    settings <- get global
    when (dsDebug settings) $ liftIO $ putStrLn msg

initialize :: WorldSystem ()
initialize = do
    TextureInfo moonTex _ <- loadTexture "resources/textures/8k_moon.png"
    program <- either error id <$> loadShader "resources/shaders/lighting"
    -- fixedLighting <- either error id <$> loadShader "resources/shaders/fixed_lighting"
    -- glyph <- either error id <$> loadShader "resources/shaders/glyph"
    -- font <- loadFont "resources/fonts/Inconsolata.ttf" 48 glyph
    TextureInfo selectTex _ <- loadTexture "resources/textures/select_circle.png"
    TextureInfo unitTex _ <- loadTexture "resources/textures/unit.png"


    debugLog "Loading gltf file"
    nodes <- getRight <$> loadGlTF "resources/models/AnimatedCube/AnimatedCube.gltf" \case 
        "POSITION" -> GL.AttribLocation 0
        "NORMAL" -> GL.AttribLocation 1
        "TANGENT" -> GL.AttribLocation 2
        "TEXCOORD_0" -> GL.AttribLocation 3
        _ -> GL.AttribLocation (-1)

    liftIO $ print nodes
    liftIO $ putStrLn "\n\n"
    -- error "stop here"

    etys <- createGlTFEntities nodes

    forM_ etys $ \ety -> Apecs.modify ety $ (,Shader program, Pos $ V3 10 0 0).(id :: Mesh -> Mesh)

    (V2 w h :: V2 Double) <- (realToFrac <$>) . screenSize <$> get global

    -- withShader glyph do
    --     "projection" $== ortho 0 (realToFrac w) (realToFrac h) 0 (-1) (1 :: Float)


    sphereMesh <- createSphereAsset 20
    planeMesh <- createPlaneAsset 2

    cameraEty <- newEntity (Camera, Pos $ V3 10 0 1, CamRot 0 0, Vel $ V3 0 0 0, Kinematic, Mass 1000)

    Apecs.set global (gigams - 1) -- 10^8

    -- _ <- newEntity (Mesh planeMesh, Color $ V4 1 0 0 1, Pos $ V3 0 0 0, RotEuler $ V3 0 0 0, Scale $ V3 1.00 0.01 0.01, Shader fixedLighting, Texture unitTex)
    -- _ <- newEntity (Mesh planeMesh, Color $ V4 0 1 0 1, Pos $ V3 0 0 0, RotEuler $ V3 0 0 0, Scale $ V3 0.01 1.00 0.01, Shader fixedLighting, Texture unitTex)
    -- _ <- newEntity (Mesh planeMesh, Color $ V4 0 0 1 1, Pos $ V3 0 0 0, RotEuler $ V3 90 0 0, Scale $ V3 0.01 1.00 0.01, Shader fixedLighting, Texture unitTex)

    -- ety <- newEntity (Mesh cube, Pos $ V3 0 0 0, Scale $ V3 1 1 1, RotEuler $ V3 0 0 0, Shader program, Texture moonTex)
    -- _ <- newEntity ( Overlay
    --                 , Pos $ V3 0 0 0, RotEuler $ V3 0 0 0, Scale $ V3 1 1 1
    --                 , Texture selectTex, Shader fixedLighting, Mesh planeMesh
    --                 , Link ety
    --                 )

    -- fontEty <- newEntity font
    -- hudEty <- newEntity ( Hud, TextBox "Hello World!" fontEty (V4 1 1 1 1) 0.5, Pos2D $ V2 10 30 )
    -- textInputEty <- newEntity ( TerminalInput
    --                           , Console, FormControl, TextInput 0
    --                           , TextBox "" fontEty (V4 1 1 1 1) 0.5
    --                           , Pos2D $ V2 40 (h - 60), Size2D $ V2 (w - 80) 30
    --                           , Command Nothing
    --                           )

    return ()

draw = do
    gameState <- get global
    case gameState of 
        Terminal -> drawTerminal
        _ -> drawObjects (Filter @Mesh) >> drawTextBoxes Hud

getTransformFrom pos rot scale = mkTransformationMat rot pos !*! scale

drawObjects :: forall tag. (Get World IO tag, Members World IO tag) => tag -> WorldSystem ()
drawObjects _ = cmapM_ $ \(Camera, Pos camPos, CamRot pitch yaw) -> do
    cmapM_ $ \(_ :: tag, Texture tex, Shader sdr, Mesh mesh, mcolor, (mtransform, mrot, mpos, mscale)) -> do
        let view = lookAt (realToFrac <$> camPos) (realToFrac <$> camPos + cameraForward pitch yaw) (V3 0 1 0 :: V3 Float)
        let proj = perspective (120 :: Float) (16.0/9) 0.001 1000000.0
        let rotMat = getRotMat $ fromMaybe (RotEuler $ V3 0 0 0) mrot
        let (Scale scale) = fromMaybe (Scale $ V3 1 1 1) mscale
        let (Pos pos) = fromMaybe (Pos $ V3 0 0 0) mpos
        let scaleMat = scaled $ point scale
        let m1 = getTransformFrom pos rotMat scaleMat
        let (Transform trans) = fromMaybe (Transform m1) mtransform
        let model = (realToFrac <$>) <$> trans                                                    
        let (Color color) = fromMaybe (Color $ V4 1 1 1 1) mcolor


        withShader sdr do
            "model"         $== (model :: M44 Float)
            "view"          $== view
            "projection"    $== proj

            "objectColor"   $== (realToFrac <$> color :: V4 Float)
            "ambientColor"  $== (V4 1.0 1.0 1.0 1.0 :: V4 Float)
            "viewPos"       $== (realToFrac <$> camPos :: V3 Float)
            "lightPos"      $== (V3 0 0 0 :: V3 Float)
            "lightColor"    $== (V4 1.0 1.0 1.0 1.0 :: V4 Float)

            liftIO $ GL.activeTexture $= GL.TextureUnit 0
            liftIO $ GL.textureBinding GL.Texture2D $= Just tex

            drawMeshAsset mesh -- size of the cube array
    -- liftIO exitSuccess

adjustOverlays = cmapM_ \(Camera, Pos camPos) -> do
    cmapM \(Overlay, Pos _, Link ety) -> do
        (Pos newPos) <- get ety
        let delta = camPos - newPos
        let dist = norm delta
        let dir = normalize delta
        let mat = matFromDir dir $ V3 0 1 0
        let s = dist * 0.05
        return (Overlay, Pos newPos, RotMat mat, Scale $ V3 s s s)


matFromDir :: V3 Double -> V3 Double -> M33 Double
matFromDir forward up = transpose mat
    where 
        side = cross forward up
        upn = cross forward side
        mat = V3 side upn forward

moveCamera :: (Double -> Double -> V3 Double) -> Double -> WorldSystem ()
moveCamera dir vec = cmap \(Camera, Pos pos, CamRot pitch yaw, GameSettings {cameraSpeed=(_, speed)}, Vel vel) ->
    Vel $ dir pitch yaw ^* vec ^* speed + vel

stopCam = do
    DebugState {dsCameraMomentum} <- Apecs.get global
    cmap \(Camera, Vel pos) -> if dsCameraMomentum then Vel pos else Vel 0

inertia :: Double -> WorldSystem ()
inertia delta = cmapM \(Pos p, Vel v) -> do
    -- debugLog $ "updating velocity " <> show (v ^* realToFrac delta)
    return $ Pos $ p + (v ^* delta)

handle :: Event -> WorldSystem ()
handle event = do
    textInput event
    Apecs.get global >>= \case 
        Playing -> playingInput event
        Terminal -> terminalInput event (Apecs.set global Playing)
        _ -> return ()


playingInput :: Event -> WorldSystem ()
playingInput (KeyEvent Key'Slash i ks mods) = Apecs.set global Terminal >> initTerminal
playingInput (KeyEvent Key'Escape i KeyState'Released mods) = liftIO exitSuccess
playingInput (KeyEvent Key'W i ks mods) = ifHeld ks (moveCamera cameraForward 1) stopCam
playingInput (KeyEvent Key'A i ks mods) = ifHeld ks (moveCamera cameraRight (-1)) stopCam
playingInput (KeyEvent Key'S i ks mods) = ifHeld ks (moveCamera cameraForward (-1)) stopCam
playingInput (KeyEvent Key'D i ks mods) = ifHeld ks (moveCamera cameraRight 1) stopCam
playingInput (KeyEvent Key'Q i ks mods) = ifHeld ks (moveCamera cameraUp 1) stopCam
playingInput (KeyEvent Key'E i ks mods) = ifHeld ks (moveCamera cameraUp (-1)) stopCam
playingInput (KeyEvent Key'E i ks mods) = ifHeld ks (moveCamera cameraUp (-1)) stopCam

playingInput (KeyEvent Key'F1 i KeyState'Released mods) = Apecs.modify global (\ds -> ds { dsDebug = not $ dsDebug ds })
playingInput (KeyEvent Key'F2 i KeyState'Released mods) = do
    ds <- Apecs.get global
    let pm = case dsPolygonMode ds of
            GL.Fill -> GL.Line
            GL.Line -> GL.Point
            GL.Point -> GL.Fill
    GL.polygonMode $= (pm, pm)
    Apecs.set global ds { dsPolygonMode = pm }
playingInput (KeyEvent Key'F3 i KeyState'Released mods) = Apecs.modify global (\ds -> ds { dsCameraMomentum = not $ dsCameraMomentum ds })
playingInput (KeyEvent Key'F4 i KeyState'Released mods) = do
    Apecs.modify global (\ds -> ds { dsCursorEnabled = not $ dsCursorEnabled ds })
    DebugState {dsCursorEnabled} <- Apecs.get global
    cmapM_ \win -> 
        if dsCursorEnabled then liftIO $ GLFW.setCursorInputMode win GLFW.CursorInputMode'Normal
                        else liftIO $ GLFW.setCursorInputMode win GLFW.CursorInputMode'Disabled

playingInput (CursorEvent x y) = do
    (CursorPos px py, GameSettings {cameraSpeed=(speed, _)}) <- get global

    let dx = realToFrac (x - px) * speed
    let dy = realToFrac (py - y) * speed
    cmap \(Camera, CamRot pitch yaw) -> (Camera, CamRot (clamp (-89) 89 (pitch + dy)) (yaw + dx), CursorPos x y)
playingInput _ = do
    return ()

runOrbit :: Double -> WorldSystem ()
runOrbit delta = cmap inner
    where 
        inner (Orbit rot rev rotS revS c r, Pos _) = ( Orbit rot' rev' rotS revS c r, Pos pos', RotQuat rotQ)
            where
                rot' = rot + rotS * realToFrac delta
                rev' = rev + revS * realToFrac delta
                x = r * sin rev'
                z = r * cos rev'
                pos' = V3 x 0 z
                rotQ = axisAngle (V3 0 1 0) rot'

runConsole :: Double -> WorldSystem ()
runConsole delta = return ()

step :: Double -> WorldSystem ()
step delta = do
    gameState <- get global
    adjustOverlays
    case gameState of
        Playing -> do
            runOrbit delta
            stepGravity delta
            inertia delta
            updateHud
        Terminal -> runConsole delta
        _ -> return ()



stepGravity :: Double -> WorldSystem ()
stepGravity deltaT = do
    Mantissa mant <- Apecs.get global
    GameSettings {gravConstant} <- Apecs.get global
    let g = gravConstant -- with some scaling factor :-/
    cmapM \(Kinematic, Mass kMass, Pos kPos, Vel kVel) -> do
        let accumulate acc (Mass sMass, Pos sPos) = do
                let delta = kPos - sPos
                let r = norm delta * (10^^mant)
                let dir = normalize delta
                let forceM = g * kMass * sMass / r / r
                let force = - dir ^* (forceM / (10^^mant))
                return $ if r > 0 then acc + force else acc 

        forceVec <- cfoldM accumulate (V3 0 0 0)
        let newVel = kVel + (realToFrac deltaT / kMass) *^ forceVec
        
        return (Kinematic, Vel newVel)
    -- for each kinematic 
    -- fold a sum over every thing with mass and calculate the force
    -- apply force

updateHud :: WorldSystem ()
updateHud = do
    Mantissa mant <- Apecs.get global
    (Vel cVel, Pos cPos) <- cfold (\_ (Camera, Vel cVel, Pos cPos) -> (Vel cVel, Pos cPos)) (Vel 0, Pos 0)
    let V3 vx vy vz = printToSpeed.(*10^^mant) <$> cVel
    let V3 px py pz = printToDist.(*10^^mant) <$> cPos
    cmapM \(Hud, TextBox t c s) -> do
        let str = printf "Velocity: %s %s %s Position: %s %s %s" vx vy vz px py pz
        return (Hud, TextBox str c s)

printToDist :: Double -> String
printToDist x 
    | abs x < 1000 = printf "%5.5fm " x
    | abs x < 10^^6 = printf "%5.5fkm" (x/(10^^3))
    | abs x < 10^^9 = printf "%5.5fGm" (x/(10^^6))
    | abs x < 10^^12 = printf "%5.5fTm" (x/(10^^9))
    | abs x < 10^^15 = printf "%5.5fPm" (x/(10^^12))
    | otherwise = printf "%5.5fEm" (x/(10^^15))

c = 299792458 -- m/s

printToSpeed :: Double -> String
printToSpeed x 
    | abs x < 1000 = printf "%5.5fm/s " x
    | abs x < 10^^6 = printf "%5.5fkm/s" (x/(10^^3))
    | abs x < c / 100 = printf "%5.5fGm/s" (x/(10^^6))
    | otherwise = printf "%5.5fc" (x/c)


clamp :: (Ord a) => a -> a -> a -> a
clamp min max x | x < min = min
                | x > max = max
                | otherwise = x


-- getAll :: forall w m c. (Members w m c, Get w m c) => SystemT w m [c]
-- getAll = do
--     s :: Storage c <- getStore
--     members <- lift $ explMembers s
--     mapM (get.Entity) $ V.toList members