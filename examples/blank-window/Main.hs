
module Main where

import Engine
import Linear

main :: IO ()
main = do
    win <- initWindow (V2 800 600) "Blank window"
    playIO () win (\_ -> return ()) (const return) (const return)