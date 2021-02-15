module Engine.Texture where


import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import Data.Word ( Word8 )
import Codec.Picture (readImage, convertRGBA8, Image(..))
import qualified Data.Vector.Storable as VS
import Control.Monad.IO.Class ( MonadIO(..) )

-- | Create a texture using the pixel data
createTexture   :: Int -- ^ Texture width
                -> Int -- ^ Texture height
                -> GL.PixelData a -- ^ Texture pixel data
                -> IO (GL.TextureObject, GL.TextureSize2D)
createTexture width height textureData = do
    texId <- GL.genObjectName :: IO GL.TextureObject

    GL.textureBinding GL.Texture2D $= Just texId

    let size = GL.TextureSize2D (fromIntegral width) (fromIntegral height)
    GL.texImage2D GL.Texture2D GL.NoProxy 0 GL.RGBA8 size 0 textureData

    GL.textureWrapMode GL.Texture2D GL.S $= (GL.Repeated, GL.Repeat)
    GL.textureWrapMode GL.Texture2D GL.T $= (GL.Repeated, GL.Repeat)
    GL.textureFilter GL.Texture2D $= ((GL.Nearest, Nothing), GL.Nearest)

    GL.textureBinding GL.Texture2D $= Nothing

    return (texId, size)

-- | Loads the texture from a png file
loadTexture :: MonadIO m => String -- ^ Filename
            -> m (GL.TextureObject, GL.TextureSize2D)
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