{-# LANGUAGE DuplicateRecordFields,NamedFieldPuns #-}
module Engine.Model where

    
import qualified Graphics.Rendering.OpenGL as GL
import Control.Lens.TH
import Graphics.Rendering.OpenGL (($=))
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Control.Monad.IO.Class
import Engine.Shader
import GHC.Int
import GHC.Word
import Engine.Types
import Engine.Texture
import Engine.Apecs.Types
import Linear
import Control.Monad.Trans.Except
import Data.Vector ((!), (!?))
import Control.Monad
import System.FilePath.Posix

import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString as BS

-- whoever wrote this library was rude,
-- like seriously, when are you just going to load one module
import qualified Codec.GlTF as TF
import qualified Codec.GlTF.Mesh as TF
import qualified Codec.GlTF.Accessor as TF
import qualified Codec.GlTF.BufferView as TF
import qualified Codec.GlTF.Material as TF
import qualified Codec.GlTF.Buffer as TF
import qualified Codec.GlTF.URI as TF
import qualified Codec.GlTF.Prelude as TF
import qualified Codec.GlTF.Node as TF
import qualified Codec.GlTF.Scene as TF
import qualified Codec.GlTF.Texture as TF
import qualified Codec.GlTF.Image as TF
import qualified Codec.GlTF.PbrMetallicRoughness as TF
import qualified Codec.GlTF.TextureInfo as TF

exceptFromJust :: Monad m => String -> Maybe a -> ExceptT String m a
exceptFromJust name (Just x) = return x
exceptFromJust name Nothing = throwE $ "Missing value with name: " <> name

accBufferView :: TF.Accessor -> Maybe TF.BufferViewIx
accBufferView = TF.bufferView
accComponentType :: TF.Accessor -> TF.ComponentType
accComponentType = TF.componentType
accCount :: TF.Accessor -> TF.Size
accCount = TF.count

bvByteOffset :: TF.BufferView -> TF.Size
bvByteOffset = TF.byteOffset 
bvByteLength :: TF.BufferView -> TF.Size
bvByteLength = TF.byteLength 
noScale :: TF.Node -> Maybe (Float, Float, Float)
noScale = TF.scale
scName :: TF.Scene -> Maybe T.Text
scName = TF.name
moNodes :: TF.GlTF -> Maybe (V.Vector TF.Node)
moNodes = TF.nodes
scNodes :: TF.Scene -> Maybe (V.Vector TF.NodeIx)
scNodes = TF.nodes
imUri :: TF.Image -> Maybe TF.URI
imUri = TF.uri
bUri :: TF.Buffer -> Maybe TF.URI
bUri = TF.uri

(&) = flip ($)

uncurry3 f (a,b,c) = f a b c

v2M44 [a,b,c,d, e,f,g,h, i,j,k,l, m,n,o,p] = V4 (V4 a e i m) (V4 b f j n) (V4 c g k o) (V4 d h l p)
v2M44 _ = error "wrong number of elements"

v2Quat (a,b,c,d) = Quaternion d (V3 a b c)

getRight_ :: Either a b -> b
getRight_ (Right x) = x

type GLTFBits = ( TF.Vector 
                        ( Maybe [Int]
                        , Maybe Transform
                        , Maybe (MeshAsset, GL.TextureObject, GL.TextureObject)
                        , Maybe Rot
                        , Maybe Scale
                        , Maybe Pos
                        , Maybe Scene
                        )
                    )

loadGlTF :: MonadIO m 
         => FilePath 
         -> (String -> GL.AttribLocation) 
         -> m (Either String GLTFBits)
loadGlTF filepath locations = liftIO $ runExceptT $ do

    model       <- ExceptT $ TF.fromFile filepath
    buffers     <- exceptFromJust "buffers"     $ TF.buffers model
    bufferViews <- exceptFromJust "bufferViews" $ TF.bufferViews model
    meshes      <- exceptFromJust "meshes"      $ TF.meshes model
    nodes       <- exceptFromJust "nodes"       $ moNodes model
    scenes      <- exceptFromJust "scenes"      $ TF.scenes model
    textures    <- exceptFromJust "textures"    $ TF.textures model
    materials   <- exceptFromJust "materials"   $ TF.materials model
    accessors   <- exceptFromJust "accessors"   $ TF.accessors model
    images      <- exceptFromJust "images"      $ TF.images model
    let baseDir = takeDirectory filepath


    meshAssets <- V.forM meshes \TF.Mesh {TF.primitives} ->
        -- TODO: only using the first primitive, we should use more than that
        V.head primitives & \mp@TF.MeshPrimitive {TF.attributes, TF.mode, TF.indices, TF.material} -> do
            (TF.AccessorIx indIx)   <- exceptFromJust "indices" indices
            (TF.MaterialIx matIx)   <- exceptFromJust "material" material
            indAcc                  <- exceptFromJust "indices accessor" $ accessors !? indIx
            mat                     <- exceptFromJust "material" $ materials !? matIx
            pbr                     <- exceptFromJust "pbr" $ TF.pbrMetallicRoughness mat
            -- TODO: these should probably be optional, but I dont want to worry about it now
            baseColorTexIx          <- TF.index <$> exceptFromJust "baseColorTexture index" (TF.baseColorTexture pbr)
            baseColorTexIIx         <- TF.unImageIx <$> exceptFromJust "baseColorTexture" (TF.source =<< textures !? baseColorTexIx)
            (TF.URI baseColorImageUri) <- exceptFromJust "baseColorImageUri" $ imUri =<< images !? baseColorTexIIx 
            TextureInfo baseColorImage _    <- loadTexture (baseDir </> T.unpack baseColorImageUri)

            
            metallicRoughnessTexIx  <- TF.index <$> exceptFromJust "metallicRoughnessTexture" (TF.metallicRoughnessTexture pbr)
            metallicRoughnessTexIIx <- TF.unImageIx <$> exceptFromJust "metallicRoughnessTexture" (TF.source =<< textures !? metallicRoughnessTexIx)
            (TF.URI metallicRoughnessImageUri) <- exceptFromJust "metallicRoughnessImageUri" $ imUri =<< images !? metallicRoughnessTexIIx 
            TextureInfo metallicRoughnessImage _ <- loadTexture (baseDir </> T.unpack metallicRoughnessImageUri)
            
            (TF.BufferViewIx indBVI) <- exceptFromJust "bufferView" $ accBufferView indAcc
            indBV                   <- exceptFromJust "indices bufferView" $ bufferViews !? indBVI
            let (TF.BufferIx indBI) = TF.buffer indBV
            indB                    <- exceptFromJust "indices buffer" $ buffers !? indBI
            uri                     <- exceptFromJust "uri" $ bUri indB

            bs <- ExceptT $ liftIO $ TF.loadURI ((Right<$>).BS.readFile.(baseDir</>)) uri

            vao <- GL.genObjectName
            indVBO <- GL.genObjectName
            vertVBO <- GL.genObjectName

            GL.bindVertexArrayObject $= Just vao

            liftIO $ BS.useAsCString bs \cstr -> do
                let ptr = cstr `plusPtr` bvByteOffset indBV

                GL.bindBuffer GL.ElementArrayBuffer $= Just indVBO
                GL.bufferData GL.ElementArrayBuffer $= (fromIntegral (bvByteLength indBV), ptr, GL.StaticDraw)

                GL.bindBuffer GL.ArrayBuffer $= Just vertVBO
                GL.bufferData GL.ArrayBuffer $= (fromIntegral (BS.length bs), cstr, GL.StaticDraw)
                
            forM_ (HM.toList attributes) $ \(name, TF.AccessorIx attrIx) -> do
                attrAcc                     <- exceptFromJust "attribute accessor" $ accessors !? attrIx
                (TF.BufferViewIx attrBVI)   <- exceptFromJust "attribute bufferViewIx" $ accBufferView attrAcc
                attrBV                      <- exceptFromJust "attribute bufferView" $ bufferViews !? attrBVI

                let intHandling = case accComponentType attrAcc of
                        TF.FLOAT -> GL.ToFloat
                        rest -> GL.KeepIntegral

                let dataType = case accComponentType attrAcc of 
                        TF.BYTE -> GL.Byte
                        TF.UNSIGNED_BYTE -> GL.UnsignedByte
                        TF.SHORT -> GL.Short
                        TF.UNSIGNED_SHORT -> GL.UnsignedShort
                        TF.UNSIGNED_INT -> GL.UnsignedInt
                        TF.FLOAT -> GL.Float
                        rest -> error $ "unknown accessor component type: "<>show rest 
                
                let numComp = case TF.type' attrAcc of 
                        TF.SCALAR -> 1
                        TF.VEC2 -> 2
                        TF.VEC3 -> 3
                        TF.VEC4 -> 4
                        TF.MAT2 -> 4
                        TF.MAT3 -> 9
                        TF.MAT4 -> 16
                        rest -> error $ "unknown accessor type: "<>show rest

                let attrName = T.unpack name
                let attrLoc = locations attrName

                when (attrLoc == GL.AttribLocation (-1)) $ throwE $ "cant find program attribute with name: " <> attrName

                let vertAttrPointer = (intHandling, GL.VertexArrayDescriptor numComp dataType 0 (nullPtr `plusPtr` bvByteOffset attrBV))
                liftIO $ print vertAttrPointer

                GL.vertexAttribPointer attrLoc $= vertAttrPointer
                GL.vertexAttribArray attrLoc $= GL.Enabled

            GL.bindVertexArrayObject $= Nothing
            -- GL.deleteObjectName vertVBO
            -- GL.deleteObjectName indVBO

            -- TODO: make the datatype value dependent on the file
            let meshAsset = (MeshAsset vao (fromIntegral $ accCount indAcc) Elements GL.Triangles GL.UnsignedShort)
            return (meshAsset, baseColorImage, metallicRoughnessImage)


    mconcat . V.toList <$> forM scenes \scene -> do
        sceneNodes <- exceptFromJust "scene nodes" $ scNodes scene
        forM sceneNodes $ \(TF.NodeIx nodeIx) -> do
            node <- exceptFromJust "scene node ix" $ nodes !? nodeIx
            

            return  ( V.toList . (TF.unNodeIx<$>) <$> TF.children node
                    , Transform . v2M44 . (realToFrac<$>) . V.toList . TF.unNodeMatrix <$> TF.matrix node
                    , (meshAssets !?) . TF.unMeshIx =<< TF.mesh node
                    , RotQuat . (realToFrac<$>) . v2Quat <$> TF.rotation node
                    , Scale . (realToFrac<$>) . uncurry3 V3 <$> noScale node
                    , Pos . (realToFrac<$>) . uncurry3 V3 <$> TF.translation node
                    , Scene . T.unpack <$> scName scene
                    )
