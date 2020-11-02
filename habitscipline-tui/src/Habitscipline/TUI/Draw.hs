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
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Graphics.Vty.Attributes
import Habitscipline.Data
import Habitscipline.TUI.State
import Text.Printf

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
        (createButtonAttr, fg green),
        (headerAttr, withStyle defAttr underline),
        (goodAttr, fg green),
        (selectedBothAttr, bg white),
        (selectedHabitAttr, bg black),
        (selectedDayAttr, bg black)
      ]

drawTui :: State -> [Widget ResourceName]
drawTui = \case
  StateHistory hs -> drawHistoryState hs
  StateHabitList hls -> drawHabitListState hls
  StateNewHabit nhs -> drawNewHabitState nhs

daysShown :: Integer
daysShown = 20

drawHistoryState :: HistoryState -> [Widget ResourceName]
drawHistoryState HistoryState {..} =
  [ case historyStateHabitMaps of
      Loading -> str "Loading"
      Loaded m ->
        vBox
          [ let today = historyStateMaxDay
                days = [addDays (- daysShown) today .. today]
                monthHeader d =
                  let (_, m, md) = toGregorian d
                   in if md == 1 then withDefAttr headerAttr $ str (printf "%2d" m) else str "  "
                monthsHeader = str " " : map monthHeader days
                dayHeader d =
                  let (_, _, md) = toGregorian d
                   in selectedDayModifier d $ withDefAttr headerAttr $ str (printf "%2d" md)
                daysHeader = str " " : map dayHeader days
                isSelectedDay = (== historyStateDay)
                selectedDayModifier d = if isSelectedDay d then withAttr selectedDayAttr else id
                habitRow h em =
                  let isSelectedHabit =
                        case historyStateHabitCursor of
                          Loading -> False
                          Loaded mnec -> (nonEmptyCursorCurrent <$> mnec) == Just h
                      selectedModifier = if isSelectedHabit then withAttr selectedHabitAttr else id
                   in map selectedModifier $
                        txt (habitName h)
                          : map
                            ( \d ->
                                let showAmount :: Word -> String
                                    showAmount = printf "%2d"
                                    amountWidget :: Word -> Widget ResourceName
                                    amountWidget w =
                                      let justShow = str $ showAmount w
                                       in if d == historyStateDay && isSelectedHabit
                                            then selectedTextCursorWidget ResourceTextCursor historyStateAmountCursor
                                            else justShow
                                    mAmount = case entryMapLookup em d of
                                      Exactly w -> Just w
                                      NoDataBeforeFirst -> Nothing
                                      NoDataAfterLast -> Nothing
                                      AssumedZero -> Just 0
                                    selectedBothModifier =
                                      if isSelectedDay d && isSelectedHabit
                                        then withAttr selectedBothAttr
                                        else selectedDayModifier d
                                 in selectedBothModifier $ case mAmount of
                                      Nothing -> str "  "
                                      Just a -> case habitType h of
                                        PositiveHabit ->
                                          let modifier = if a > 0 then withAttr goodAttr else id
                                           in modifier $ amountWidget a
                                        NegativeHabit ->
                                          let modifier = if a <= 0 then withAttr goodAttr else id
                                           in modifier $ amountWidget a
                            )
                            days
             in padBottom Max $ tableWidget $ monthsHeader : daysHeader : map (uncurry habitRow) (M.toList m),
            hBorder,
            padTop Max $ str "Detailed stats per habit"
          ]
  ]

drawHabitListState :: HabitListState -> [Widget ResourceName]
drawHabitListState HabitListState {..} =
  [ vBox
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
                ],
        hBorder,
        hCenterLayer $ str "Press 'n' to create a new habit"
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
              case newHabitStateCompleteHabit undefined nhs of
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

headerAttr :: AttrName
headerAttr = "header"

goodAttr :: AttrName
goodAttr = "good"

selectedBothAttr :: AttrName
selectedBothAttr = "selected-both"

selectedHabitAttr :: AttrName
selectedHabitAttr = "selected-habit"

selectedDayAttr :: AttrName
selectedDayAttr = "selected-day"
