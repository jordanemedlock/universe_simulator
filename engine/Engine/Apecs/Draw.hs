module Engine.Apecs.Draw where

import Linear hiding (trace)
import Engine.Apecs.Types
import Engine.Apecs.Logging
import Engine.Shader
import Engine.Mesh
import Engine.Camera
import Engine.ResourceManager
import Apecs
import Data.Maybe (fromMaybe)
import qualified Graphics.Rendering.OpenGL as GL

drawObjects :: forall tag w. 
            ( Get w IO tag, Members w IO tag, Get w IO Camera, Get w IO ShaderName
            , Get w IO Pos, Get w IO Mesh, Get w IO FillColor, Get w IO CamRot
            , Get w IO Transform, Get w IO Rot, Get w IO Scale, Has w IO GL.Program
            , Get w IO Logger, Members w IO Logger
            ) 
            => tag 
            -> System w ()
drawObjects _ = cmapM_ $ \(Camera, Pos camPos, CamRot pitch yaw) -> do
    trace $ "Drawing objects with camera: " <> show (Camera, Pos camPos, CamRot pitch yaw)
    cmapM_ $ \(_ :: tag, ShaderName sName, Mesh mesh, mcolor, (mtransform, mrot, mpos, mscale)) -> do
        trace $ "Drawing object with info: " <> show (ShaderName sName, mcolor, (mtransform, mrot, mpos, mscale))
        sdr <- getRes sName
        let view = lookAt (realToFrac <$> camPos) (realToFrac <$> camPos + cameraForward pitch yaw) (V3 0 1 0 :: V3 Float)
        let proj = perspective (120 :: Float) (16.0/9) 0.001 1000000.0
        let rotMat = getRotMat $ fromMaybe (RotEuler $ V3 0 0 0) mrot
        let (Scale scale) = fromMaybe (Scale $ V3 1 1 1) mscale
        let (Pos pos) = fromMaybe (Pos $ V3 0 0 0) mpos
        let scaleMat = scaled $ point scale
        let m1 = mkTransformationMat rotMat pos !*! scaleMat
        let (Transform trans) = fromMaybe (Transform m1) mtransform
        let model = (realToFrac <$>) <$> trans
        let (FillColor color) = fromMaybe (FillColor $ V4 1 1 1 1) mcolor

        withShader sdr do
            "model"         $== (model :: M44 Float)
            "view"          $== view
            "projection"    $== proj

            "objectColor"   $== color
            "ambientColor"  $== (V4 1.0 1.0 1.0 1.0 :: V4 Float)
            "viewPos"       $== (realToFrac <$> camPos :: V3 Float)
            "lightPos"      $== (V3 0 0 0 :: V3 Float)
            "lightColor"    $== (V4 1.0 1.0 1.0 1.0 :: V4 Float)

            drawMeshAsset mesh


drawSprites :: forall tag w. (Get w IO tag, Members w IO tag) 
            => tag 
            -> System w ()
drawSprites _ = do
    return () -- TODO: big fat todo
    -- cmapM_ $ \(_ :: tag, Shader sdr, mtex, mcolor, (mrot, mpos, mscale)) -> do
        

    --     withShader sdr do
    --         "model"         $== (model :: M44 Float)
    --         "view"          $== view
    --         "projection"    $== proj

    --         "objectColor"   $== color
    --         "ambientColor"  $== (V4 1.0 1.0 1.0 1.0 :: V4 Float)
    --         "viewPos"       $== (realToFrac <$> camPos :: V3 Float)
    --         "lightPos"      $== (V3 0 0 0 :: V3 Float)
    --         "lightColor"    $== (V4 1.0 1.0 1.0 1.0 :: V4 Float)

    --         drawMeshAsset mesh
