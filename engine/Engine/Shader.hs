
module Engine.Shader where


import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import qualified Data.ByteString as BS
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import System.Directory
import Control.Monad
import Control.Monad.Trans.Except
import System.FilePath.Posix

ifM :: (Monad m) => m Bool -> m a -> m a -> m a
ifM c a b = do 
    v <- c
    if v then a else b

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Right a) = Just a
eitherToMaybe _ = Nothing

readIfExists :: (MonadIO m) => FilePath -> m (Either String BS.ByteString)
readIfExists path = liftIO $ ifM (doesFileExist path) 
                                 (Right <$> BS.readFile path) 
                                 (return $ Left $ "File does not exist " <> path)

loadShader :: (MonadIO m) => String -> m (Either String GL.Program)
loadShader filePath = runExceptT do
    let vertFile = filePath <.> "vert"
    let fragFile = filePath <.> "frag"
    let geomFile = filePath <.> "geom"

    vertSource <- ExceptT $ readIfExists vertFile
    fragSource <- ExceptT $ readIfExists fragFile
    mGeomSource <- liftIO $ eitherToMaybe <$> readIfExists geomFile
    ExceptT $ compileShader vertSource fragSource mGeomSource


-- | Compiles shader from its strings
compileShader   :: (MonadIO m)
                => BS.ByteString -- ^ Vertex Soure
                -> BS.ByteString -- ^ Fragment Source
                -> Maybe BS.ByteString -- ^ Optional Geometry Source
                -> m (Either String GL.Program)
compileShader vertexSource fragmentSource mGeometrySource = liftIO do
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

            return $ Left "Failed to compile shader"
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

                    return $ Right program
                else return $ Left "Failed to link shader"

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
