
module Main where

import Engine
import Linear


data GameState = GameState 
    { shader :: Program
    , cube :: MeshAsset
    , texture :: TextureInfo
    }


main :: IO ()
main = do
    window <- initWindow (V2 1920 1080) "Textured box"

    program <- loadFromName "fixed_lighting" 
    texture <- loadFromName "test_texture.png"
    cube <- createCubeAsset

    playIO (GameState program cube texture) window draw (const return) (const return)

draw :: GameState -> IO ()
draw (GameState program cube texture) = do
    
    let view = lookAt (V3 1 1 1) (V3 0 0 0) (V3 0 1 0 :: V3 Float)
    let ratio = 1920 / 1080 :: Float
    projection <- perspectiveMatrix 45 ratio 0.1 100.0

    withShader program do
        "projection"    $== projection
        "model"         $== (identity :: M44 Float)
        "view"          $== view

        "objectColor"   $== (V4 1.0 0.5 0.3 1.0 :: V4 Float)
        "ambientColor"  $== (V4 1.0 1.0 1.0 1.0 :: V4 Float)
        "viewPos"       $== (V3 1 1 1 :: V3 Float)
        "lightPos"      $== (V3 1.1 1.0 2.0 :: V3 Float)
        "lightColor"    $== (V4 1.0 1.0 1.0 1.0 :: V4 Float)
        
        setTexture 0 texture
        drawMeshAsset cube


