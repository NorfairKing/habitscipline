{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Habitscipline.TUI.Draw where

import Brick.AttrMap
import Brick.Markup
import Brick.Types
import Brick.Util
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.Core
import Cursor.Brick
import Cursor.Simple.List.NonEmpty
import Cursor.Text
import qualified Data.Text as T
import Graphics.Vty.Attributes
import Habitscipline.Data
import Habitscipline.TUI.State

buildAttrMap :: State -> AttrMap
buildAttrMap =
  const $
    attrMap
      defAttr
      [ (nameAttr, fg white),
        (descriptionAttr, defAttr),
        (typeAttr, fg yellow),
        (unitAttr, fg green),
        (numeratorAttr, fg magenta),
        (denominatorAttr, fg blue),
        (selectedAttr, fg white),
        (errorAttr, fg red),
        (cancelButtonAttr, fg yellow),
        (createButtonAttr, fg green)
      ]

drawTui :: State -> [Widget ResourceName]
drawTui = \case
  StateHabitList hls -> drawHabitListState hls
  StateNewHabit nhs -> drawNewHabitState nhs

drawHabitListState :: HabitListState -> [Widget ResourceName]
drawHabitListState HabitListState {..} =
  [ hBox
      [ case habitListStateHabits of
          Loading -> centerLayer $ str "Loading"
          Loaded mCursor -> case mCursor of
            Nothing -> hCenterLayer $ str "No habits yet, press 'n' to create one."
            Just cursor ->
              hBox
                [ let go = (: []) . txtWrap . habitName
                   in padAll 1 $ verticalNonEmptyCursorTable go (map (withDefAttr selectedAttr) . go) go cursor,
                  vBorder,
                  let Habit {..} = nonEmptyCursorCurrent cursor
                      Goal {..} = habitGoal
                   in padAll 1 $
                        vBox
                          [ hBox [str "Name: ", withAttr nameAttr $ txtWrap habitName],
                            hBox [str "Description: ", withAttr descriptionAttr $ maybe emptyWidget txtWrap habitDescription],
                            borderWithLabel (str "[ Goal ]") $ padLeftRight 1 $
                              vBox
                                [ hBox [str "Unit: ", txtWrap goalUnit],
                                  hBox [str "Numerator: ", txtWrap (T.pack (show goalNumerator))],
                                  hBox [str "Numerator: ", txtWrap (T.pack (show goalDenominator))],
                                  padTop (Pad 1)
                                    $ markup
                                    $ mconcat
                                    $ case habitType of
                                      PositiveHabit ->
                                        [ "I want to achieve ",
                                          T.pack (show goalNumerator) @? numeratorAttr,
                                          " ",
                                          goalUnit @? unitAttr,
                                          " every ",
                                          T.pack (show goalDenominator) @? denominatorAttr,
                                          " days."
                                        ]
                                      NegativeHabit ->
                                        [ "I want to have at most ",
                                          T.pack (show goalNumerator) @? numeratorAttr,
                                          " ",
                                          goalUnit @? unitAttr,
                                          " every ",
                                          T.pack (show goalDenominator) @? denominatorAttr,
                                          " days."
                                        ]
                                ]
                          ]
                ]
      ]
  ]

drawNewHabitState :: NewHabitState -> [Widget ResourceName]
drawNewHabitState nhs@NewHabitState {..} =
  let selIf sel = if newHabitStateSelection == sel then withDefAttr selectedAttr else id
      textWithSelection sel =
        if newHabitStateSelection == sel
          then selectedTextCursorWidget ResourceTextCursor
          else textCursorWidget
   in [ borderWithLabel (str "[ Habit ]")
          $ padLeftRight 1
          $ vBox
            [ selIf SelectName $
                hBox
                  [ str "Name: ",
                    withAttr nameAttr $ textWithSelection SelectName newHabitStateName
                  ],
              selIf SelectDescription $
                hBox
                  [ str "Description: ",
                    withAttr descriptionAttr $ textWithSelection SelectDescription newHabitStateDescription
                  ],
              selIf SelectType $
                hBox
                  [ str "Type: ",
                    withAttr typeAttr $ txt $ renderHabitType newHabitStateType
                  ],
              borderWithLabel (str "[ Goal ]")
                $ padLeftRight 1
                $ vBox
                  [ selIf SelectGoalUnit $
                      hBox
                        [ str "Unit: ",
                          withAttr unitAttr $ textWithSelection SelectGoalUnit newHabitStateGoalUnit
                        ],
                    selIf SelectGoalNumerator $
                      hBox
                        [ str "Numerator: ",
                          withAttr numeratorAttr $ textWithSelection SelectGoalNumerator newHabitStateGoalNumerator
                        ],
                    selIf SelectGoalDenominator $
                      hBox
                        [ str "Denominator: ",
                          withAttr denominatorAttr $ textWithSelection SelectGoalDenominator newHabitStateGoalDenominator
                        ],
                    padTop (Pad 1) $ markup
                      $ mconcat
                      $ case newHabitStateType of
                        PositiveHabit ->
                          [ "I want to achieve ",
                            rebuildTextCursor newHabitStateGoalNumerator @? numeratorAttr,
                            " ",
                            rebuildTextCursor newHabitStateGoalUnit @? unitAttr,
                            " every ",
                            rebuildTextCursor newHabitStateGoalDenominator @? denominatorAttr,
                            " days."
                          ]
                        NegativeHabit ->
                          [ "I want to have at most ",
                            rebuildTextCursor newHabitStateGoalNumerator @? numeratorAttr,
                            " ",
                            rebuildTextCursor newHabitStateGoalUnit @? unitAttr,
                            " every ",
                            rebuildTextCursor newHabitStateGoalDenominator @? denominatorAttr,
                            " days."
                          ]
                  ],
              case newHabitStateCompleteHabit nhs of
                Left t -> withDefAttr errorAttr $ borderWithLabel (str "[ Error ]") $ padLeftRight 1 $ txt t
                Right _ -> emptyWidget,
              hBox
                [ selIf SelectCancelButton $ border $ padLeftRight 1 $ withAttr cancelButtonAttr $ str "Cancel",
                  selIf SelectCreateButton $ border $ padLeftRight 1 $ withAttr createButtonAttr $ str "Create"
                ],
              str "Tab: Next, Shift-Tab: Previous"
            ]
      ]

nameAttr :: AttrName
nameAttr = "name"

descriptionAttr :: AttrName
descriptionAttr = "description"

typeAttr :: AttrName
typeAttr = "type"

numeratorAttr :: AttrName
numeratorAttr = "numerator"

denominatorAttr :: AttrName
denominatorAttr = "denominator"

unitAttr :: AttrName
unitAttr = "unit"

cancelButtonAttr :: AttrName
cancelButtonAttr = "cancel-button"

createButtonAttr :: AttrName
createButtonAttr = "create-button"

selectedAttr :: AttrName
selectedAttr = "selected"

errorAttr :: AttrName
errorAttr = "error"
