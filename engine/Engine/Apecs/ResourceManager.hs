module Engine.Apecs.ResourceManager where

import Apecs
import Engine

data ResourceManagers = 
    ResourceManagers 
        (ResourceManager GL.Program)
        (ResourceManager (GL.TextureObject, GL.TextureSize2D))
        (ResourceManager Font)
        (ResourceManager GLTFBits)

instance Semigroup ResourceManagers where
    (ResourceManagers a b c d) <> (ResourceManagers a' b' c' d') = 
        ResourceManagers (a <> a') (b <> b') (c <> c') (d <> d')

instance Monoid ResourceManagers where
    mempty = ResourceManagers mempty mempty mempty mempty


-- instance (Get w m ResourceManagers, Set w m ResourceManagers, Resource m a) => HasResource (SystemT w m) a where
--     getResource name = do
--         (ResourceManager hm) <- get global

--         case HM.lookup name hm of 
--             Nothing -> do
--                 res <- loadFromName name
--                 set global $ HM.insert name (ResourceManagers)
--                 return res
--             Just res -> return res
