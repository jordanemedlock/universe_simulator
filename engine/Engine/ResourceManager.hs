{-# LANGUAGE TypeFamilies #-}
module Engine.ResourceManager where


import qualified Data.HashMap.Strict as HM
import Control.Monad.IO.Class
import Data.IORef
import Data.String
import Data.Typeable
import qualified Graphics.Rendering.OpenGL as GL
import Apecs.Core
import Engine.Shader
import Engine.Texture
import Engine.Model
import Engine.Font
import Engine.Types

class Resource a where
   loadFromName :: MonadIO m => String -> m a

class Monad m => ExplGetStr m s where
   explGetStr :: s -> String -> m (Elem s)
   explStrExists :: s -> String -> m Bool
class Monad m => ExplSetStr m s where
   explSetStr :: s -> String -> Elem s -> m ()
class ResoureManager s where
   getResource :: MonadIO m => s -> String -> m (Elem s)

newtype StrMap c = StrMap (IORef (HM.HashMap String c))
type instance Elem (StrMap c) = c
instance MonadIO m => ExplInit m (StrMap c) where
   explInit = liftIO $ StrMap <$> newIORef mempty
instance (MonadIO m, Typeable c) => ExplGetStr m (StrMap c) where
   explGetStr (StrMap ref) name = liftIO $ flip fmap (HM.lookup name <$> readIORef ref) $ \case
    Just c -> c
    notFound -> error $ unwords
      [ "Reading non-existent Map component"
      , show (typeRep notFound)
      , "for entity"
      , show name
      ]
   explStrExists (StrMap ref) name = liftIO $ HM.member name <$> readIORef ref
instance (MonadIO m) => ExplSetStr m (StrMap c) where 
   explSetStr (StrMap ref) name value = liftIO $ 
      modifyIORef ref (HM.insert name value)
instance (Resource a, Typeable a) => ResoureManager (StrMap a) where
   getResource sm name = do
      exists <- explStrExists sm name
      if exists 
         then explGetStr sm name
         else do
            res <- loadFromName name
            explSetStr sm name res
            return res

getRes :: (MonadIO m, Has w m c, ResoureManager (Storage c)) => String -> SystemT w m c
getRes name = do
   store <- getStore
   getResource store name


instance Resource GL.Program where
    loadFromName name = do
        eprog <- loadShader ("resources/shaders/"<>name)
        case eprog of 
            Left m -> error m
            Right p -> return p


instance Resource TextureInfo where
    loadFromName = loadTexture . ("resources/textures/"<>)


instance Resource GLTFBits where
    loadFromName name = getRight_ <$> loadGlTF ("resources/models/"<>name<>"/"<>name<>".gltf") \case 
        "POSITION" -> GL.AttribLocation 0
        "NORMAL" -> GL.AttribLocation 1
        "TANGENT" -> GL.AttribLocation 3
        "TEXCOORD_0" -> GL.AttribLocation 2
        _ -> GL.AttribLocation (-1)


instance Resource Font where
    -- TODO: gotta figure out how to not hardcode those :-/ 
    -- maybe not, who careds
    loadFromName name = loadFont ("resources/fonts/"<>name) 48 =<< loadFromName "glyph"

instance Component GL.Program where type Storage GL.Program = StrMap GL.Program
instance Component GLTFBits where type Storage GLTFBits = StrMap GLTFBits
instance Component Font where type Storage Font = StrMap Font
instance Component TextureInfo where type Storage TextureInfo = StrMap  TextureInfo
