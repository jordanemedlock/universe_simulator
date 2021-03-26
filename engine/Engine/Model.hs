module Engine.Model where

import Control.Monad.Trans.Except
import Codec.GlTF as G

-- fromMaybe :: (Monad m) => String -> Maybe a -> ExceptT String m a
-- fromMaybe msg x = ExceptT $ return $ maybeToRight (msg<>" doesnt exist") x

loadBuffer :: String -> Buffer -> IO (Either String ByteString)
loadBuffer root buffer = case G.uri buffer of
    Nothing -> return $ Left "uri doesnt exist"
    Just x -> loadURI x



loadMeshes :: MonadIO m => GlTF -> String -> m (Either String (V.Vector MeshAsset))
loadMeshes model root = runExceptT do
    let meshes = fromMaybe [] $ G.meshes model
    let buffers = fromMaybe [] $ G.buffers model
    let bufferViews = fromMaybe [] $ G.bufferViews model
    let accessors = fromMaybe [] $ G.accessors model

    buffersBS <- liftIO $ mapM (loadBuffer root) buffers
