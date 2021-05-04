{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE TypeApplications      #-}

module Engine.Apecs.Terminal where

import Engine.Apecs.Types
import Engine.Apecs.FormControl
import qualified Graphics.UI.GLFW as GLFW
import Apecs
import Linear
import Engine.Types

type HasTerminal w = ( HasTextBox w
                     , HasFormControl w, Set w IO FormControl
                     , Get w IO Command, Set w IO Command
                     , Get w IO Console, Members w IO Console, Set w IO Console
                     , Get w IO Size2D, Set w IO Size2D
                     , Get w IO Pos2D, Set w IO Pos2D
                     , Get w IO TerminalInput, Members w IO TerminalInput
                     , Get w IO EntityCounter
                     , Get w IO Focus, Set w IO Focus, Destroy w IO Focus
                     )

terminalInput :: (HasTerminal w) => Event -> System w () -> System w ()
terminalInput (KeyEvent GLFW.Key'Escape i GLFW.KeyState'Released mods) leave = do
    cmap \(TerminalInput, TextBox t co s) -> (FormControl, Not @Focus, TextInput 0, TextBox "" co s)
    leave

terminalInput (KeyEvent GLFW.Key'Enter i GLFW.KeyState'Released mods) _ = 
    cmapM \(TerminalInput, TextBox t co s, Command mprev, Pos2D (V2 x y), Size2D size) -> do
        -- runCommand t
        let consoleDeltaHeight = 40
        cmap \(Console, TextBox {}, Pos2D (V2 x y)) -> (Console, Pos2D (V2 x (y - consoleDeltaHeight)))
        newPrevEty <- newEntity ( Console
                                , TextBox t co s
                                , Pos2D $ V2 x (y - consoleDeltaHeight), Size2D size
                                , Command mprev
                                )
        return (TextBox "" co s, Command $ Just newPrevEty, Pos2D $ V2 x y)
terminalInput _ _ = return ()

-- pushCommand :: String -> System World ()
-- pushCommand msg = cmapM_ 

initTerminal :: HasTerminal w => System w ()
initTerminal = cmap \(Console, FormControl, TextInput _) -> (FormControl, Focus)

drawTerminal :: HasTerminal w => System w ()
drawTerminal = drawTextBoxes Console