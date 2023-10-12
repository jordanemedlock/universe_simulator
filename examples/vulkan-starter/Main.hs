module Main where

import Engine
import           Control.Exception
import           Data.Foldable
import           Text.Pretty.Simple
import           Vulkan.Core10
import           Vulkan.Zero
import Vulkan.Extensions.VK_KHR_portability_enumeration
import qualified Linear as L
import Foreign.C.String (peekCString)
import qualified Data.Vector as V
import qualified Graphics.UI.GLFW as GLFW
import qualified Data.ByteString                   as BS
import           Vulkan.Extensions.VK_EXT_validation_features
import           Vulkan.Extensions.VK_EXT_debug_utils
import           Vulkan.Utils.Debug
import           Vulkan.CStruct.Extends
import           Data.Bits


errorCallback :: Show a => a -> [Char] -> IO ()
errorCallback err msg = do
    putStrLn "GLFW Error Occurred: "
    print err 
    putStrLn $ "with message: " ++ msg


mainLoop i = do
    return ()
main :: IO ()
main = do
    window <- initWindow (L.V2 700 700) "Vulkan"
    availableLayerNames <- toList . fmap layerName . snd <$> enumerateInstanceLayerProperties
    print availableLayerNames
    let layers = ["VK_LAYER_KHRONOS_validation"]
    glfwExtensions <- mapM BS.packCString =<< GLFW.getRequiredInstanceExtensions
    print glfwExtensions

    let extensions = glfwExtensions <> [KHR_PORTABILITY_ENUMERATION_EXTENSION_NAME, EXT_DEBUG_UTILS_EXTENSION_NAME]
    let createInfo = zero 
            { applicationInfo = Just zero 
                { applicationName = Just "Vulkan test"
                , applicationVersion = API_VERSION_1_0
                , engineName = Just "Engine"
                , engineVersion = API_VERSION_1_0
                , apiVersion = API_VERSION_1_0
                }
            , enabledLayerNames = V.fromList layers
            , enabledExtensionNames = V.fromList $ extensions
            , flags = INSTANCE_CREATE_ENUMERATE_PORTABILITY_BIT_KHR
            } 
            ::& debugUtilsMessengerCreateInfo
            :& ()
    withInstance createInfo Nothing bracket mainLoop


debugUtilsMessengerCreateInfo :: DebugUtilsMessengerCreateInfoEXT
debugUtilsMessengerCreateInfo = zero
  { messageSeverity = DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT
                        .|. DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT
  , messageType     = DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT
                      .|. DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT
                      .|. DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT
  , pfnUserCallback = debugCallbackPtr
  }