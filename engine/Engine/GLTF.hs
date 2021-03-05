{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FunctionalDependencies #-}
module Engine.GLTF where

import Data.Vector
import Control.Lens.TH
import Data.Aeson
import Data.Text (Text)


data Asset = Asset deriving Show
makeFields ''Asset

instance FromJSON Asset where
    parseJSON = withObject "Asset" $ const $ return Asset


data Accessor = Accessor deriving Show
makeFields ''Accessor

instance FromJSON Accessor where
    parseJSON = withObject "Accessor" $ const $ return Accessor


data Buffer = Buffer { _bufferUri :: Maybe Text
                     } deriving Show 
makeFields ''Buffer

instance FromJSON Buffer where
    parseJSON = withObject "Buffer" $ \v -> Buffer
        <$> v .: "uri"


data BufferView = BufferView { _bufferViewByteLength :: Int
                             , _bufferViewByteOffset :: Int
                             , _bufferViewBuffer :: Int
                             } deriving Show
makeFields ''BufferView

instance FromJSON BufferView where
    parseJSON = withObject "BufferView" $ \v -> BufferView
        <$> v .: "byteLength"
        <*> v .: "byteOffset"
        <*> v .: "buffer"

data Mesh = Mesh deriving Show
makeFields ''Mesh

instance FromJSON Mesh where
    parseJSON = withObject "Mesh" $ const $ return Mesh


data Material = Material deriving Show
makeFields ''Material

instance FromJSON Material where
    parseJSON = withObject "Material" $ const $ return Material


data Texture = Texture deriving Show
makeFields ''Texture

instance FromJSON Texture where
    parseJSON = withObject "Texture" $ const $ return Texture


data Image = Image deriving Show
makeFields ''Image

instance FromJSON Image where
    parseJSON = withObject "Image" $ const $ return Image


data GLTF = GLTF { _gLTFAsset :: Asset
                 , _gLTFAccessors :: Maybe (Vector Accessor)
                 , _gLTFBuffers :: Maybe (Vector Buffer)
                 , _gLTFBufferViews :: Maybe (Vector BufferView)
                 , _gLTFMeshes :: Maybe (Vector Mesh)
                 , _gLTFMaterials :: Maybe (Vector Material)
                 , _gLTFTextures :: Maybe (Vector Texture)
                 , _gLTFImages :: Maybe (Vector Image)
                 } deriving (Show)
makeFields ''GLTF

instance FromJSON GLTF where
    parseJSON = withObject "GLTF" $ \v -> GLTF
        <$> v .: "asset"
        <*> v .: "accessors"
        <*> v .: "buffers"
        <*> v .: "bufferViews"
        <*> v .: "meshes"
        <*> v .: "materials"
        <*> v .: "textures"
        <*> v .: "images"

