module Engine.Texture where


import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import Data.Word ( Word8 )
import Codec.Picture (readImage, convertRGBA8, Image(..))
import qualified Data.Vector.Storable as VS
import Control.Monad.IO.Class ( MonadIO(..) )
import Linear
import Foreign.Marshal.Array
import Foreign.Ptr
import Engine.Shader

data TextureInfo = TextureInfo GL.TextureObject GL.TextureSize2D deriving (Show)

-- | Create a texture using the pixel data
createTexture   :: Int -- ^ Texture width
                -> Int -- ^ Texture height
                -> GL.PixelData a -- ^ Texture pixel data
                -> IO TextureInfo -- (GL.TextureObject, GL.TextureSize2D)
createTexture width height textureData = do
    texId <- GL.genObjectName :: IO GL.TextureObject

    GL.textureBinding GL.Texture2D $= Just texId

    let size = GL.TextureSize2D (fromIntegral width) (fromIntegral height)
    GL.texImage2D GL.Texture2D GL.NoProxy 0 GL.RGBA8 size 0 textureData

    GL.textureWrapMode GL.Texture2D GL.S $= (GL.Repeated, GL.Repeat)
    GL.textureWrapMode GL.Texture2D GL.T $= (GL.Repeated, GL.Repeat)
    GL.textureFilter GL.Texture2D $= ((GL.Nearest, Nothing), GL.Nearest)

    GL.textureBinding GL.Texture2D $= Nothing

    return $ TextureInfo texId size

unitTexture :: MonadIO m => V4 Float -> m GL.TextureObject
unitTexture (V4 r g b a) = liftIO do
    color <- newArray (floor.(*256) <$> [r, g, b, a] :: [Word8])
    texId <- GL.genObjectName :: IO GL.TextureObject

    GL.textureBinding GL.Texture2D $= Just texId

    let pixelData = GL.PixelData GL.RGBA GL.UnsignedByte color

    GL.texImage2D GL.Texture2D GL.NoProxy 0 GL.RGBA8 (GL.TextureSize2D 1 1) 0 pixelData
    
    GL.textureWrapMode GL.Texture2D GL.S $= (GL.Repeated, GL.Repeat)
    GL.textureWrapMode GL.Texture2D GL.T $= (GL.Repeated, GL.Repeat)
    GL.textureFilter GL.Texture2D $= ((GL.Nearest, Nothing), GL.Nearest)

    GL.textureBinding GL.Texture2D $= Nothing

    return texId

-- | Loads the texture from a png file
loadTexture :: MonadIO m => String -- ^ Filename
            -> m TextureInfo
loadTexture filename = liftIO do
    eimage <- readImage filename

    case eimage of
        Left msg -> do
            putStrLn msg
            error msg
        Right image -> do
            let rgbPixel = convertRGBA8 image
            
            let width = fromIntegral $ imageWidth rgbPixel
            let height = fromIntegral $ imageHeight rgbPixel
            let idata = imageData rgbPixel :: VS.Vector Word8

            VS.unsafeWith idata 
                (createTexture width height . GL.PixelData GL.RGBA GL.UnsignedByte)

setTexture :: MonadIO m => Int -> TextureInfo -> m ()
setTexture texUnit (TextureInfo tex _) = liftIO do
    GL.activeTexture $= GL.TextureUnit (fromIntegral texUnit)
    GL.textureBinding GL.Texture2D $= Just tex

unsetTexture :: MonadIO m => Int -> m ()
unsetTexture texUnit = liftIO do
    GL.activeTexture $= GL.TextureUnit (fromIntegral texUnit)
    GL.textureBinding GL.Texture2D $= Nothing
    