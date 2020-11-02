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
import qualified Data.Map as M
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
      [ (nameAttr, fg yellow),
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
        (todayAttr, fg magenta),
        (selectedBothAttr, bg white)
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
                  let (_, month, md) = toGregorian d
                   in if md == 1 then withDefAttr headerAttr $ str (printf "%2d" month) else str "  "
                monthsHeader = map monthHeader days ++ [str " "]
                dayHeader d =
                  let (_, _, md) = toGregorian d
                   in withDefAttr (if d == historyStateToday then todayAttr else headerAttr) $ str (printf "%2d" md)
                daysHeader = map dayHeader days ++ [str " "]
                isSelectedDay = (== historyStateDay)
                isSelectedHabit h =
                  case historyStateHabitCursor of
                    Loading -> False
                    Loaded mnec -> (nonEmptyCursorCurrent <$> mnec) == Just (habitUuid h)
                amountCell h em d =
                  let showAmount :: Word -> String
                      showAmount w =
                        if habitBoolean h
                          then case habitType h of
                            PositiveHabit ->
                              if w > 0
                                then " ✓"
                                else "  "
                            NegativeHabit ->
                              if w > 0
                                then " ✗"
                                else " ✓"
                          else printf "%2d" w
                      amountWidget :: Maybe Word -> Widget ResourceName
                      amountWidget mw =
                        if isSelectedDay d && isSelectedHabit h
                          then forceAttr selectedBothAttr $ selectedTextCursorWidget ResourceTextCursor historyStateAmountCursor
                          else case mw of
                            Nothing -> str "  "
                            Just w -> str $ showAmount w
                      mAmount = case entryMapLookup em d of
                        Exactly w -> Just w
                        NoDataBeforeFirst -> Nothing
                        NoDataAfterLast -> Just 0 -- Assume 0 so that it doesn't require extra effort from the user
                        AssumedZero -> Just 0
                      isGood a = case habitType h of
                        PositiveHabit -> a > 0
                        NegativeHabit -> a <= 0
                      goodModifier a = if isGood a then withAttr goodAttr else id
                   in case mAmount of
                        Nothing -> amountWidget Nothing
                        Just a -> goodModifier a $ amountWidget $ Just a
                habitRow h em = map (amountCell h em) days ++ [padLeft (Pad 1) $ withAttr nameAttr $ txt (habitName h)]
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

todayAttr :: AttrName
todayAttr = "today"

selectedBothAttr :: AttrName
selectedBothAttr = "selected-both"
