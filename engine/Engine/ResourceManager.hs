module Engine.ResourceManager (
    Symbol, -- I want to hide the constructor so the only way to make it is with a literal
    Resource(..),
    HasResource(..),
    ResourceManager(..)
) where


import qualified Data.HashMap.Strict as HM
import Control.Monad.IO.Class
import Data.IORef
import Data.String


class Monad m => ExplGetStr m s where
   explGetStr :: s -> String -> m (Elem s)
   explStrExists :: s -> String -> m Bool
class Monad m => ExplSetStr m s where
   explSetStr :: s -> String -> Elem s -> m ()
class Monad m => ResoureManager m s where
   getResource :: s -> String -> m (Elem s)

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