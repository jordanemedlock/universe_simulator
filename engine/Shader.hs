
module Shader where


import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import qualified Data.ByteString as BS
import System.FilePath.Posix (takeExtensions)




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
