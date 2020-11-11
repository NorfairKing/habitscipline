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
import Control.Monad
import Cursor.Brick
import Cursor.Simple.List.NonEmpty
import Cursor.Text
import Data.List
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
        (booleanAttr, fg red),
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
        (selectedBothAttr, bg white),
        (goalMetAttr, withStyle (fg green) underline),
        (goalNotMetAttr, defAttr)
      ]

drawTui :: State -> [Widget ResourceName]
drawTui = \case
  StateHistory hs -> drawHistoryState hs
  StateHabitList hls -> drawHabitListState hls
  StateNewHabit nhs -> drawNewHabitState nhs

daysShown :: Integer
daysShown = 20

days :: Day -> [Day]
days maxDay = [addDays (- daysShown) maxDay .. maxDay]

header :: Day -> Day -> Widget n
header today maxDay =
  let ds = days maxDay
      dayColumn d =
        let (_, month, md) = toGregorian d
            dow = dayOfWeek d
            withTodayAttr =
              if d == today
                then withDefAttr todayAttr
                else id
         in vBox
              [ str $
                  if md == 1
                    then printf "%2d" month
                    else "  ",
                withTodayAttr $ txt $
                  case dow of
                    Monday -> "Mo"
                    Tuesday -> "Tu"
                    Wednesday -> "We"
                    Thursday -> "Th"
                    Friday -> "Fr"
                    Saturday -> "Sa"
                    Sunday -> "Su",
                withTodayAttr $ str (printf "%2d" md)
              ]
   in hBox . intersperse (str " ") $ map dayColumn ds

habitRow :: Day -> Day -> Load (Maybe (NonEmptyCursor HabitUuid)) -> TextCursor -> Habit -> EntryMap -> Widget ResourceName
habitRow selectedDay maxDay habitCursor amountCursor h em =
  let g@Goal {..} = habitGoal h
      ds = days maxDay
      isSelectedDay = (== selectedDay)
      isSelectedHabit =
        case habitCursor of
          Loading -> False
          Loaded mnec -> (nonEmptyCursorCurrent <$> mnec) == Just (habitUuid h)
      amountCell :: Day -> Widget ResourceName
      amountCell d =
        let met = entryMapGoalMet g d em
            withMetAttr = case met of
              Nothing -> id
              Just True -> withDefAttr goalMetAttr
              Just False -> withDefAttr goalNotMetAttr
            showAmount :: Maybe Word -> String
            showAmount = \case
              Nothing -> "  "
              Just w ->
                if goalBoolean
                  then case goalType of
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
              hLimit 2 $ padLeft Max $
                if isSelectedDay d && isSelectedHabit
                  then
                    forceAttr selectedBothAttr $
                      selectedTextCursorWidget
                        ResourceTextCursor
                        amountCursor
                  else str $ showAmount mw
            mAmount = case entryMapLookup em d of
              Exactly w -> Just w
              NoDataBeforeFirst -> Nothing
              NoDataAfterLast -> Just 0 -- Assume 0 so that it doesn't require extra effort from the user
              AssumedZero -> Just 0
            isGood a = case goalType of
              PositiveHabit -> a > 0
              NegativeHabit -> a <= 0
            goodModifier a = if isGood a then withAttr goodAttr else id
         in withMetAttr $ case mAmount of
              Nothing -> amountWidget Nothing
              Just a -> goodModifier a $ amountWidget $ Just a
      spacerCell :: Day -> Day -> Widget n
      spacerCell d1 d2 = ($ str " ") $ case (entryMapGoalMet g d1 em, entryMapGoalMet g d2 em) of
        (Nothing, Nothing) -> id
        (Just True, Nothing) -> id
        (Just False, Nothing) -> id
        (Nothing, Just True) -> id
        (Nothing, Just False) -> id
        (Just True, Just True) -> withAttr goalMetAttr
        (Just True, Just False) -> withAttr goalNotMetAttr
        (Just False, Just True) -> withAttr goalNotMetAttr
        (Just False, Just False) -> withAttr goalNotMetAttr
      goDays [] = [padLeft (Pad 1) $ withAttr nameAttr $ txt (habitName h)]
      goDays [d] = amountCell d : goDays []
      goDays (d1 : d2 : rest) = amountCell d1 : spacerCell d1 d2 : goDays (d2 : rest)
   in hBox $ goDays ds

drawHistoryState :: HistoryState -> [Widget ResourceName]
drawHistoryState HistoryState {..} =
  [ centerLayer $ borderWithLabel (str "[ Habitscipline ]") $ case historyStateHabitMaps of
      Loading -> str "Loading"
      Loaded m ->
        padLeftRight 2 $ padAll 1 $
          vBox
            [ vBox $
                header historyStateToday historyStateMaxDay
                  : map (uncurry (habitRow historyStateDay historyStateMaxDay historyStateHabitCursor historyStateAmountCursor)) (M.toList m),
              padTop (Pad 1) $ case historyStateHabitCursor of
                Loading -> emptyWidget
                Loaded mnec -> case nonEmptyCursorCurrent <$> mnec of
                  Nothing -> emptyWidget
                  Just uuid -> case find ((== uuid) . habitUuid . fst) (M.toList m) of
                    Nothing -> emptyWidget
                    Just (h@Habit {..}, em) ->
                      let longestStreak = entryMapLongestStreak habitGoal em historyStateToday
                          latestStreak = entryMapLatestStreak habitGoal em historyStateToday
                          currentStreak = do
                            latest <- latestStreak
                            guard (streakIsCurrent latest historyStateToday)
                            pure latest
                       in vBox
                            [ drawHabitDescription h,
                              padTop (Pad 1) $
                                vBox
                                  [ str $ "Longest streak: " <> maybe "0" (show . streakDays) longestStreak,
                                    str $ "Latest streak: " <> maybe "" (show . streakDays) latestStreak,
                                    str $ "Current streak: " <> maybe "" (show . streakDays) currentStreak
                                  ]
                            ]
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
                [ let go = (: []) . txt . habitName
                   in padAll 1 $ verticalNonEmptyCursorTable go (map (withDefAttr selectedAttr) . go) go cursor,
                  vBorder,
                  padAll 1 $ drawHabitDescription $ nonEmptyCursorCurrent cursor
                ],
        hBorder,
        hCenterLayer $
          vBox
            [ str "Press 'n' to create a new habit",
              str "Press 'e' to edit a habit",
              str "Press 'h' to go to the history overview"
            ]
      ]
  ]

drawHabitDescription :: Habit -> Widget n
drawHabitDescription Habit {..} =
  let Goal {..} = habitGoal
   in vBox
        [ hBox [str "Name: ", withAttr nameAttr $ txt habitName],
          hBox [str "Description: ", withAttr descriptionAttr $ maybe emptyWidget txt habitDescription],
          hBox
            [ str "Goal: ",
              markup
                $ mconcat
                $ case goalType of
                  PositiveHabit ->
                    [ "I want to achieve ",
                      T.pack (show goalNumerator) @? numeratorAttr,
                      " ",
                      habitUnit @? unitAttr,
                      " every ",
                      T.pack (show goalDenominator) @? denominatorAttr,
                      " days."
                    ]
                  NegativeHabit ->
                    [ "I want to have at most ",
                      T.pack (show goalNumerator) @? numeratorAttr,
                      " ",
                      habitUnit @? unitAttr,
                      " every ",
                      T.pack (show goalDenominator) @? denominatorAttr,
                      " days."
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
              selIf SelectUnit $
                hBox
                  [ str "Unit: ",
                    withAttr unitAttr $ textWithSelection SelectUnit newHabitStateUnit
                  ],
              selIf SelectGoalType $
                hBox
                  [ str "Type: ",
                    withAttr typeAttr $ txt $ renderHabitType newHabitStateGoalType
                  ],
              selIf SelectGoalBoolean $
                hBox
                  [ str "Boolean: ",
                    withAttr booleanAttr $ txt $ if newHabitStateGoalBoolean then "Yes" else "No"
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
                $ case newHabitStateGoalType of
                  PositiveHabit ->
                    [ "I want to achieve ",
                      rebuildTextCursor newHabitStateGoalNumerator @? numeratorAttr,
                      " ",
                      rebuildTextCursor newHabitStateUnit @? unitAttr,
                      " every ",
                      rebuildTextCursor newHabitStateGoalDenominator @? denominatorAttr,
                      " days."
                    ]
                  NegativeHabit ->
                    [ "I want to have at most ",
                      rebuildTextCursor newHabitStateGoalNumerator @? numeratorAttr,
                      " ",
                      rebuildTextCursor newHabitStateUnit @? unitAttr,
                      " every ",
                      rebuildTextCursor newHabitStateGoalDenominator @? denominatorAttr,
                      " days."
                    ],
              case newHabitStateCompleteHabit undefined nhs of
                Left t -> withDefAttr errorAttr $ borderWithLabel (str "[ Error ]") $ padLeftRight 1 $ txt t
                Right _ -> emptyWidget,
              hBox
                [ selIf SelectCancelButton $ border $ padLeftRight 1 $ withAttr cancelButtonAttr $ str "Cancel",
                  selIf SelectCreateButton $ border $ padLeftRight 1 $ withAttr createButtonAttr $ str $ case newHabitStateHabit of
                    Nothing -> "Create"
                    Just _ -> "Save"
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

booleanAttr :: AttrName
booleanAttr = "boolean"

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

goalMetAttr :: AttrName
goalMetAttr = "goal-met"

goalNotMetAttr :: AttrName
goalNotMetAttr = "goal-not-met"
