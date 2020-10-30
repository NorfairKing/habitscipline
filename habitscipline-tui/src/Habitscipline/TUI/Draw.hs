{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Habitscipline.TUI.Draw where

import Brick.AttrMap
import Brick.Types
import Brick.Widgets.Border
import Brick.Widgets.Core
import Cursor.Brick
import Graphics.Vty.Attributes
import Habitscipline.TUI.State

buildAttrMap :: State -> AttrMap
buildAttrMap = const $ attrMap defAttr []

drawTui :: State -> [Widget ResourceName]
drawTui = \case
  StateHabitList hls -> drawHabitListState hls
  StateNewHabit nhs -> drawNewHabitState nhs

drawHabitListState :: HabitListState -> [Widget ResourceName]
drawHabitListState s = [strWrap $ show s]

drawNewHabitState :: NewHabitState -> [Widget ResourceName]
drawNewHabitState NewHabitState {..} =
  let textWithSelection sel =
        if newHabitStateSelection == sel
          then selectedTextCursorWidget ResourceTextCursor
          else textCursorWidget
   in [ borderWithLabel (str "Habit") $
          vBox
            [ hBox
                [ str "Name: ",
                  textWithSelection SelectName newHabitStateName
                ],
              hBox
                [ str "Description: ",
                  textWithSelection SelectDescription newHabitStateDescription
                ],
              hBox
                [ str "Type: ",
                  str $ show newHabitStateType
                ],
              borderWithLabel (str "Goal") $
                vBox
                  [ hBox
                      [ str "Unit: ",
                        textWithSelection SelectGoalUnit newHabitStateGoalUnit
                      ],
                    hBox
                      [ str "Numerator: ",
                        textWithSelection SelectGoalNumerator newHabitStateGoalNumerator
                      ],
                    hBox
                      [ str "Denominator: ",
                        textWithSelection SelectGoalDenominator newHabitStateGoalDenominator
                      ]
                  ]
            ]
      ]
