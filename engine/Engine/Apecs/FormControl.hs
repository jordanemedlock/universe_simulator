{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE ConstraintKinds       #-}
module Engine.Apecs.FormControl where

import Apecs
import Linear
import qualified Graphics.UI.GLFW as GLFW
import Engine.Apecs.Types
import Engine.Events
import Engine.Font
import Engine.Types

type HasTextBox w = (Get w IO TextBox, Members w IO TextBox, Get w IO Pos2D, Members w IO Pos2D, Get w IO Font)
type HasTag tag w = (Get w IO tag, Members w IO tag)
type HasFormControl w = ( Get w IO TextBox
                        , Members w IO TextBox, Set w IO TextBox
                        , Get w IO FormControl, Members w IO FormControl
                        , Get w IO TextInput, Members w IO TextInput, Set w IO TextInput
                        , Get w IO Focus, Members w IO Focus
                        )

drawTextBoxes :: forall tag. forall w. (HasTag tag w, HasTextBox w) => tag -> System w ()
drawTextBoxes _ = cmapM_ \(_ :: tag, TextBox t f c s, Pos2D pos) -> get f >>= \f -> renderString f t pos s c
    

textInput :: (HasTextBox w, HasFormControl w) => Event -> System w ()
textInput (KeyEvent GLFW.Key'Delete i ks mods) = whenReleased ks $
    cmap \(FormControl, Focus, TextInput index, TextBox t f co s) -> (TextInput index, TextBox (take index t <> drop (index+1) t) f co s)
textInput (KeyEvent GLFW.Key'Backspace i ks mods) = whenReleased ks $
    cmap \(FormControl, Focus, TextInput index, TextBox t f co s) -> (TextInput $ index - 1, TextBox (take (index-1) t <> drop index t) f co s)
textInput (CharEvent c) = cmap \(FormControl, Focus, TextInput index, TextBox t f co s) -> (TextInput $ index + 1, TextBox (t<>[c]) f co s)
textInput _ = return ()