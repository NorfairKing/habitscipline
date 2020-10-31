module Habitscipline.TUI.Handle where

import Brick.BChan
import Brick.Main
import Brick.Types
import Control.Monad.IO.Class
import Cursor.Simple.List.NonEmpty
import Cursor.Text
import Cursor.Types
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Graphics.Vty.Input.Events
import Habitscipline.Data
import Habitscipline.TUI.Env
import Habitscipline.TUI.State

handleTuiEvent :: BChan Request -> State -> BrickEvent n Response -> EventM n (Next State)
handleTuiEvent chan s e =
  case s of
    StateHabitList hls -> handleHabitListState chan hls e
    StateNewHabit nhs -> handleNewHabitState chan nhs e

handleHabitListState :: BChan Request -> HabitListState -> BrickEvent n Response -> EventM n (Next State)
handleHabitListState chan s e =
  case e of
    VtyEvent vtye ->
      let cursorDo func = case habitListStateHabits s of
            Loading -> continue $ StateHabitList s
            Loaded mCursor -> case mCursor of
              Nothing -> continue $ StateHabitList s
              Just cursor ->
                let cursor' = fromMaybe cursor $ func cursor
                 in continue $ StateHabitList $ s {habitListStateHabits = Loaded $ Just cursor'}
       in case vtye of
            EvKey KEsc [] -> halt $ StateHabitList s
            EvKey (KChar 'q') [] -> halt $ StateHabitList s
            EvKey (KChar 'r') [] -> toHabitList chan -- refresh
            EvKey (KChar 'n') [] -> toNewHabit
            EvKey KDown [] -> cursorDo nonEmptyCursorSelectNext
            EvKey (KChar 'j') [] -> cursorDo nonEmptyCursorSelectNext
            EvKey KUp [] -> cursorDo nonEmptyCursorSelectPrev
            EvKey (KChar 'k') [] -> cursorDo nonEmptyCursorSelectPrev
            _ -> continue $ StateHabitList s
    AppEvent resp -> case resp of
      ResponseHabits hs -> continue $ StateHabitList $ s {habitListStateHabits = Loaded $ makeNonEmptyCursor <$> NE.nonEmpty hs}
    _ -> continue $ StateHabitList s

handleNewHabitState :: BChan Request -> NewHabitState -> BrickEvent n Response -> EventM n (Next State)
handleNewHabitState chan s e =
  case e of
    VtyEvent vtye -> case newHabitStateSelection s of
      SelectName ->
        case vtye of
          EvKey KEsc [] -> toHabitList chan
          EvKey (KChar '\t') [] -> continue $ StateNewHabit $ s {newHabitStateSelection = SelectDescription}
          EvKey KBackTab [] -> continue $ StateNewHabit $ s {newHabitStateSelection = SelectCreateButton}
          _ -> fmap (\tc -> StateNewHabit $ s {newHabitStateName = tc}) <$> handleTC (newHabitStateName s) e
      SelectDescription ->
        case vtye of
          EvKey KEsc [] -> toHabitList chan
          EvKey (KChar '\t') [] -> continue $ StateNewHabit $ s {newHabitStateSelection = SelectType}
          EvKey KBackTab [] -> continue $ StateNewHabit $ s {newHabitStateSelection = SelectName}
          _ -> fmap (\tc -> StateNewHabit $ s {newHabitStateDescription = tc}) <$> handleTC (newHabitStateDescription s) e
      SelectType ->
        case vtye of
          EvKey KEsc [] -> toHabitList chan
          EvKey (KChar '\t') [] -> continue $ StateNewHabit $ s {newHabitStateSelection = SelectGoalUnit}
          EvKey KBackTab [] -> continue $ StateNewHabit $ s {newHabitStateSelection = SelectDescription}
          EvKey KLeft [] -> continue $ StateNewHabit $ s {newHabitStateType = PositiveHabit}
          EvKey KRight [] -> continue $ StateNewHabit $ s {newHabitStateType = NegativeHabit}
          _ -> continue $ StateNewHabit s
      SelectGoalUnit ->
        case vtye of
          EvKey KEsc [] -> toHabitList chan
          EvKey (KChar '\t') [] -> continue $ StateNewHabit $ s {newHabitStateSelection = SelectGoalNumerator}
          EvKey KBackTab [] -> continue $ StateNewHabit $ s {newHabitStateSelection = SelectType}
          _ -> fmap (\tc -> StateNewHabit $ s {newHabitStateGoalUnit = tc}) <$> handleTC (newHabitStateGoalUnit s) e
      SelectGoalNumerator ->
        case vtye of
          EvKey KEsc [] -> toHabitList chan
          EvKey (KChar '\t') [] -> continue $ StateNewHabit $ s {newHabitStateSelection = SelectGoalDenominator}
          EvKey KBackTab [] -> continue $ StateNewHabit $ s {newHabitStateSelection = SelectGoalUnit}
          _ -> fmap (\tc -> StateNewHabit $ s {newHabitStateGoalNumerator = tc}) <$> handleTC (newHabitStateGoalNumerator s) e
      SelectGoalDenominator ->
        case vtye of
          EvKey KEsc [] -> toHabitList chan
          EvKey (KChar '\t') [] -> continue $ StateNewHabit $ s {newHabitStateSelection = SelectCancelButton}
          EvKey KBackTab [] -> continue $ StateNewHabit $ s {newHabitStateSelection = SelectGoalNumerator}
          _ -> fmap (\tc -> StateNewHabit $ s {newHabitStateGoalDenominator = tc}) <$> handleTC (newHabitStateGoalDenominator s) e
      SelectCancelButton ->
        case vtye of
          EvKey KEsc [] -> toHabitList chan
          EvKey KEnter [] -> toHabitList chan
          EvKey (KChar '\t') [] -> continue $ StateNewHabit $ s {newHabitStateSelection = SelectCreateButton}
          EvKey KBackTab [] -> continue $ StateNewHabit $ s {newHabitStateSelection = SelectGoalDenominator}
          _ -> continue $ StateNewHabit s
      SelectCreateButton ->
        case vtye of
          EvKey KEsc [] -> toHabitList chan
          EvKey KEnter [] ->
            case newHabitStateCompleteHabit s of
              Left _ -> continue $ StateNewHabit s
              Right h -> do
                liftIO $ writeBChan chan $ RequestCreateHabit h
                toHabitList chan
          EvKey (KChar '\t') [] -> continue $ StateNewHabit $ s {newHabitStateSelection = SelectName}
          EvKey KBackTab [] -> continue $ StateNewHabit $ s {newHabitStateSelection = SelectCancelButton}
          _ -> continue $ StateNewHabit s
    _ -> continue $ StateNewHabit s

handleTC :: TextCursor -> BrickEvent n Response -> EventM n (Next TextCursor)
handleTC tc e =
  let textDo func = continue $ fromMaybe tc $ func tc
   in case e of
        VtyEvent vtye -> case vtye of
          EvKey (KChar c) [] -> textDo $ textCursorInsert c
          EvKey KBS [] -> textDo $ dullMDelete . textCursorRemove
          EvKey KDel [] -> textDo $ dullMDelete . textCursorDelete
          EvKey KLeft [] -> textDo textCursorSelectPrev
          EvKey KRight [] -> textDo textCursorSelectNext
          _ -> continue tc
        _ -> continue tc

toHabitList :: BChan Request -> EventM n (Next State)
toHabitList chan = do
  liftIO $ writeBChan chan RequestHabits
  continue $
    StateHabitList
      HabitListState
        { habitListStateHabits = Loading
        }

toNewHabit :: EventM n (Next State)
toNewHabit =
  continue $
    StateNewHabit
      NewHabitState
        { newHabitStateName = emptyTextCursor,
          newHabitStateDescription = emptyTextCursor,
          newHabitStateType = PositiveHabit,
          newHabitStateGoalUnit = emptyTextCursor,
          newHabitStateGoalNumerator = emptyTextCursor,
          newHabitStateGoalDenominator = emptyTextCursor,
          newHabitStateSelection = SelectName
        }
