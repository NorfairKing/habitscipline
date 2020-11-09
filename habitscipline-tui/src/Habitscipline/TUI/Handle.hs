module Habitscipline.TUI.Handle where

import Brick.BChan
import Brick.Main
import Brick.Types
import Control.Monad.IO.Class
import Cursor.Simple.List.NonEmpty
import Cursor.Text
import Cursor.Types
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import Data.Time
import Data.UUID.Typed
import Graphics.Vty.Input.Events
import Habitscipline.Data
import Habitscipline.TUI.Draw
import Habitscipline.TUI.Env
import Habitscipline.TUI.State
import Text.Read

handleTuiEvent :: BChan Request -> State -> BrickEvent n Response -> EventM n (Next State)
handleTuiEvent chan s e =
  case s of
    StateHistory hs -> handleHistoryState chan hs e
    StateHabitList hls -> handleHabitListState chan hls e
    StateNewHabit nhs -> handleNewHabitState chan nhs e

handleHistoryState :: BChan Request -> HistoryState -> BrickEvent n Response -> EventM n (Next State)
handleHistoryState chan s e =
  case e of
    VtyEvent vtye ->
      let cursorDo func = case historyStateHabitCursor s of
            Loading -> continue $ StateHistory s
            Loaded mCursor -> case mCursor of
              Nothing -> continue $ StateHistory s
              Just cursor ->
                let cursor' = fromMaybe cursor $ func cursor
                 in continue $ StateHistory $
                      s
                        { historyStateHabitCursor = Loaded $ Just cursor',
                          historyStateAmountCursor = emptyTextCursor
                        }
          dayDo func =
            let d = min (historyStateToday s) $ func $ historyStateDay s
             in continue $ StateHistory $
                  s
                    { historyStateDay = d,
                      historyStateMaxDay = min (addDays daysShown d) $ max d (historyStateMaxDay s)
                    }
       in case vtye of
            EvKey KEsc [] -> halt $ StateHistory s
            EvKey KEnter [] ->
              case historyStateHabitCursor s of
                Loaded (Just nec) -> case readMaybe $ T.unpack $ rebuildTextCursor $ historyStateAmountCursor s of
                  Just amount -> do
                    liftIO $ writeBChan chan $ RequestSetEntry $
                      Entry
                        { entryHabit = nonEmptyCursorCurrent nec,
                          entryDay = historyStateDay s,
                          entryAmount = amount
                        }
                    continue $ StateHistory $ s {historyStateAmountCursor = emptyTextCursor}
                  Nothing -> continue $ StateHistory s
                _ -> continue $ StateHistory s
            EvKey (KChar 'q') [] -> halt $ StateHistory s
            EvKey (KChar 'h') [] -> toHabitList chan
            EvKey KDown [] -> cursorDo nonEmptyCursorSelectNext
            EvKey (KChar 'j') [] -> cursorDo nonEmptyCursorSelectNext
            EvKey KUp [] -> cursorDo nonEmptyCursorSelectPrev
            EvKey (KChar 'k') [] -> cursorDo nonEmptyCursorSelectPrev
            EvKey KLeft [] -> dayDo $ addDays (-1)
            EvKey KRight [] -> dayDo $ addDays 1
            _ -> fmap (\tc -> StateHistory $ s {historyStateAmountCursor = tc}) <$> handleTC (historyStateAmountCursor s) e
    AppEvent resp -> case resp of
      ResponseHistory hms ->
        continue $ StateHistory $
          s
            { historyStateHabitMaps = Loaded hms,
              historyStateHabitCursor = case historyStateHabitCursor s of
                Loaded c -> Loaded c -- Don't reload the cursor if it was already loaded
                Loading -> Loaded $ makeNonEmptyCursor . fmap habitUuid <$> NE.nonEmpty (M.keys hms)
            }
      _ -> continue $ StateHistory s
    _ -> continue $ StateHistory s

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
            EvKey (KChar 'r') [] -> toHabitList chan
            EvKey (KChar 'h') [] -> toHistory chan
            EvKey (KChar 'n') [] -> toNewHabit
            EvKey KDown [] -> cursorDo nonEmptyCursorSelectNext
            EvKey (KChar 'j') [] -> cursorDo nonEmptyCursorSelectNext
            EvKey KUp [] -> cursorDo nonEmptyCursorSelectPrev
            EvKey (KChar 'k') [] -> cursorDo nonEmptyCursorSelectPrev
            _ -> continue $ StateHabitList s
    AppEvent resp -> case resp of
      ResponseHabits hs -> continue $ StateHabitList $ s {habitListStateHabits = Loaded $ makeNonEmptyCursor <$> NE.nonEmpty hs}
      _ -> continue $ StateHabitList s
    _ -> continue $ StateHabitList s

handleNewHabitState :: BChan Request -> NewHabitState -> BrickEvent n Response -> EventM n (Next State)
handleNewHabitState chan s e =
  case e of
    VtyEvent vtye -> case newHabitStateSelection s of
      SelectName ->
        case vtye of
          EvKey KEsc [] -> toHabitList chan
          EvKey KBackTab [] -> continue $ StateNewHabit $ s {newHabitStateSelection = SelectCreateButton}
          EvKey (KChar '\t') [] -> continue $ StateNewHabit $ s {newHabitStateSelection = SelectDescription}
          _ -> fmap (\tc -> StateNewHabit $ s {newHabitStateName = tc}) <$> handleTC (newHabitStateName s) e
      SelectDescription ->
        case vtye of
          EvKey KEsc [] -> toHabitList chan
          EvKey KBackTab [] -> continue $ StateNewHabit $ s {newHabitStateSelection = SelectName}
          EvKey (KChar '\t') [] -> continue $ StateNewHabit $ s {newHabitStateSelection = SelectUnit}
          _ -> fmap (\tc -> StateNewHabit $ s {newHabitStateDescription = tc}) <$> handleTC (newHabitStateDescription s) e
      SelectUnit ->
        case vtye of
          EvKey KEsc [] -> toHabitList chan
          EvKey KBackTab [] -> continue $ StateNewHabit $ s {newHabitStateSelection = SelectDescription}
          EvKey (KChar '\t') [] -> continue $ StateNewHabit $ s {newHabitStateSelection = SelectGoalType}
          _ -> fmap (\tc -> StateNewHabit $ s {newHabitStateUnit = tc}) <$> handleTC (newHabitStateUnit s) e
      SelectGoalType ->
        case vtye of
          EvKey KEsc [] -> toHabitList chan
          EvKey KBackTab [] -> continue $ StateNewHabit $ s {newHabitStateSelection = SelectUnit}
          EvKey (KChar '\t') [] -> continue $ StateNewHabit $ s {newHabitStateSelection = SelectGoalBoolean}
          EvKey KLeft [] -> continue $ StateNewHabit $ s {newHabitStateGoalType = PositiveHabit}
          EvKey KRight [] -> continue $ StateNewHabit $ s {newHabitStateGoalType = NegativeHabit}
          _ -> continue $ StateNewHabit s
      SelectGoalBoolean ->
        case vtye of
          EvKey KEsc [] -> toHabitList chan
          EvKey KBackTab [] -> continue $ StateNewHabit $ s {newHabitStateSelection = SelectGoalType}
          EvKey (KChar '\t') [] -> continue $ StateNewHabit $ s {newHabitStateSelection = SelectGoalNumerator}
          EvKey KLeft [] -> continue $ StateNewHabit $ s {newHabitStateGoalBoolean = False}
          EvKey KRight [] -> continue $ StateNewHabit $ s {newHabitStateGoalBoolean = True}
          _ -> continue $ StateNewHabit s
      SelectGoalNumerator ->
        case vtye of
          EvKey KEsc [] -> toHabitList chan
          EvKey KBackTab [] -> continue $ StateNewHabit $ s {newHabitStateSelection = SelectGoalBoolean}
          EvKey (KChar '\t') [] -> continue $ StateNewHabit $ s {newHabitStateSelection = SelectGoalDenominator}
          _ -> fmap (\tc -> StateNewHabit $ s {newHabitStateGoalNumerator = tc}) <$> handleTC (newHabitStateGoalNumerator s) e
      SelectGoalDenominator ->
        case vtye of
          EvKey KEsc [] -> toHabitList chan
          EvKey KBackTab [] -> continue $ StateNewHabit $ s {newHabitStateSelection = SelectGoalNumerator}
          EvKey (KChar '\t') [] -> continue $ StateNewHabit $ s {newHabitStateSelection = SelectCancelButton}
          _ -> fmap (\tc -> StateNewHabit $ s {newHabitStateGoalDenominator = tc}) <$> handleTC (newHabitStateGoalDenominator s) e
      SelectCancelButton ->
        case vtye of
          EvKey KEsc [] -> toHabitList chan
          EvKey KEnter [] -> toHabitList chan
          EvKey KBackTab [] -> continue $ StateNewHabit $ s {newHabitStateSelection = SelectGoalDenominator}
          EvKey (KChar '\t') [] -> continue $ StateNewHabit $ s {newHabitStateSelection = SelectCreateButton}
          _ -> continue $ StateNewHabit s
      SelectCreateButton ->
        case vtye of
          EvKey KEsc [] -> toHabitList chan
          EvKey KEnter [] -> do
            uuid <- liftIO nextRandomUUID
            case newHabitStateCompleteHabit uuid s of
              Left _ -> continue $ StateNewHabit s
              Right h -> do
                liftIO $ writeBChan chan $ RequestCreateHabit h
                toHabitList chan
          EvKey KBackTab [] -> continue $ StateNewHabit $ s {newHabitStateSelection = SelectCancelButton}
          EvKey (KChar '\t') [] -> continue $ StateNewHabit $ s {newHabitStateSelection = SelectName}
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

toHistory :: BChan Request -> EventM n (Next State)
toHistory chan = do
  liftIO $ writeBChan chan RequestHistory
  today <- liftIO $ utctDay <$> getCurrentTime
  continue $ StateHistory $
    HistoryState
      { historyStateHabitMaps = Loading,
        historyStateHabitCursor = Loading,
        historyStateAmountCursor = emptyTextCursor,
        historyStateToday = today,
        historyStateDay = today,
        historyStateMaxDay = today
      }

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
          newHabitStateUnit = emptyTextCursor,
          newHabitStateGoalType = PositiveHabit,
          newHabitStateGoalBoolean = True,
          newHabitStateGoalNumerator = emptyTextCursor,
          newHabitStateGoalDenominator = emptyTextCursor,
          newHabitStateSelection = SelectName
        }
