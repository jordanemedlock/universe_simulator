module Engine.Apecs.Logging where


import Engine.Apecs.Types
import Control.Monad.IO.Class
import Apecs
import System.IO
import System.Console.Pretty

log'    :: (Get w IO Logger, Members w IO Logger) 
        => LogLevel 
        -> String 
        -> System w ()
log' level msg = cmapM_ $ \(Logger levels loc) -> do
    if level `elem` levels
        then logMsg loc $ " * [" <> (levelMod loc level $ levelMsg level) <> "] " <> msg
        else return ()

trace, debug, warn, error, fatal, info :: (Get w IO Logger, Members w IO Logger) 
        => String 
        -> System w ()
trace = log' LogTrace
debug = log' LogDebug
warn = log' LogWarn
error = log' LogError
fatal = log' LogFatal
info = log' LogInfo

logMsg  :: MonadIO m 
        => LogLoc
        -> String 
        -> m ()
logMsg LogStdOut msg = liftIO $ hPutStrLn stdout msg
logMsg LogStdErr msg = liftIO $ hPutStrLn stderr msg
logMsg (LogFile filepath) msg = liftIO $ withFile filepath AppendMode $ \handle -> do
    hPutStrLn handle msg

levelMsg LogTrace = "TRACE"
levelMsg LogDebug = "DEBUG"
levelMsg LogWarn = "WARN"
levelMsg LogError = "ERROR"
levelMsg LogFatal = "FATAL"
levelMsg LogInfo = "INFO"

levelMod LogStdOut LogTrace   = style Normal  . color Green
levelMod LogStdOut LogDebug   = style Normal  . color Blue 
levelMod LogStdOut LogWarn    = style Normal  . color Yellow
levelMod LogStdOut LogError   = style Normal  . color Red
levelMod LogStdOut LogFatal   = style Bold    . color Red
levelMod LogStdOut LogInfo    = style Normal  . color Cyan
levelMod _ _ = id