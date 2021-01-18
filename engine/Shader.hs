
module Shader where


import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import qualified Data.ByteString as BS
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class


-- | Compiles shader from its strings
compileShader   :: BS.ByteString -- ^ Vertex Soure
                -> BS.ByteString -- ^ Fragment Source
                -> Maybe BS.ByteString -- ^ Optional Geometry Source
                -> IO (Maybe GL.Program)
compileShader vertexSource fragmentSource mGeometrySource = do
    vertex <- GL.createShader GL.VertexShader
    fragment <- GL.createShader GL.FragmentShader

    GL.shaderSourceBS vertex $= vertexSource
    GL.shaderSourceBS fragment $= fragmentSource

    GL.compileShader vertex
    GL.compileShader fragment

    vertexStatus <- GL.get $ GL.compileStatus vertex
    fragmentStatus <- GL.get $ GL.compileStatus fragment

    (geometryStatus, mGeometry) <- case mGeometrySource of
        Just geometrySource -> do
            geometry <- GL.createShader GL.GeometryShader

            GL.shaderSourceBS geometry $= geometrySource

            GL.compileShader geometry

            geometryStatus <- GL.get $ GL.compileStatus geometry

            return (geometryStatus, Just geometry)
        Nothing -> return (True, Nothing)

    if not $ vertexStatus && fragmentStatus && geometryStatus
        then do
            vertexInfoLog <- GL.get $ GL.shaderInfoLog vertex
            fragmentInfoLog <- GL.get $ GL.shaderInfoLog fragment
            geometryInfoLog <- case mGeometry of
                Just geometry -> GL.get $ GL.shaderInfoLog geometry
                Nothing -> return ""

            putStrLn "Failed to compile shaders with message: "
            putStrLn $ "Vertex Shader: " ++ vertexInfoLog
            putStrLn $ "Fragment Shader: " ++ fragmentInfoLog
            putStrLn $ "Geometry Shader: " ++ geometryInfoLog

            return Nothing
        else do
            program <- GL.createProgram

            GL.attachShader program vertex
            GL.attachShader program fragment
            mapM_ (GL.attachShader program) mGeometry

            GL.linkProgram program

            programStatus <- GL.get $ GL.linkStatus program

            if programStatus
                then do
                    GL.deleteObjectNames $ [vertex, fragment] ++ maybe [] (:[]) mGeometry

                    return $ Just program
                else return Nothing

type ShaderMonad = ReaderT GL.Program
type VAOMonad = ReaderT GL.VertexArrayObject
type VBOMonad = ReaderT GL.BufferObject

withShader :: MonadIO m => GL.Program -> ShaderMonad m a -> m a
withShader prog f = do
    liftIO $ GL.currentProgram $= Just prog
    value <- runReaderT f prog
    liftIO $ GL.currentProgram $= Nothing
    return value

($==) :: (GL.Uniform a, MonadIO m) => String -> a -> ShaderMonad m ()
name $== value = do
    prog <- ask
    location <- liftIO $ GL.uniformLocation prog name
    liftIO $ GL.uniform location $= value


withVAO :: MonadIO m => GL.VertexArrayObject -> VAOMonad m a -> m a
withVAO vao f = do
    liftIO $ GL.bindVertexArrayObject $= Just vao
    value <- runReaderT f vao
    liftIO $ GL.bindVertexArrayObject $= Nothing
    return value

initVAO :: MonadIO m => VAOMonad m () -> m GL.VertexArrayObject
initVAO f = do
    vao <- liftIO GL.genObjectName
    withVAO vao f
    return vao


withVBO :: MonadIO m => GL.BufferObject -> GL.BufferTarget -> VBOMonad m a -> m a
withVBO vbo target f = do
    liftIO $ GL.bindBuffer target $= Just vbo
    value <- runReaderT f vbo
    liftIO $ GL.bindBuffer target $= Nothing
    return value

tempVBO :: MonadIO m => GL.BufferTarget -> VBOMonad m a -> m a
tempVBO target f = do
    vbo <- liftIO GL.genObjectName
    value <- withVBO vbo target f
    liftIO $ GL.deleteObjectName vbo
    return value
