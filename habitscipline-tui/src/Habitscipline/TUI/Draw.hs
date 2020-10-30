module Habitscipline.TUI.Draw where

import Brick.AttrMap
import Brick.Types
import Brick.Widgets.Core
import Graphics.Vty.Attributes
import Habitscipline.TUI.State

buildAttrMap :: State -> AttrMap
buildAttrMap = const $ attrMap defAttr []

drawTui :: State -> [Widget ResourceName]
drawTui s = [str $ show s]
