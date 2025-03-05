module Maw.Main where

import Control.Monad
import Control.Monad.State (StateT (..), lift)
import Control.Monad.State qualified as State
import Data.Bits ((.|.))
import Data.Foldable (traverse_)
import Data.List qualified as List
import Data.Maybe qualified as Maybe
import Graphics.X11 qualified as X
import Graphics.X11.Xlib.Extras qualified as X
import Maw.Command
import Safe (headMay)

data ManagedWindow = ManagedWindow
    { window :: X.Window
    , dimensions :: (Int, Int)
    }
    deriving stock (Show)

data WMState = WMState
    { windows :: [ManagedWindow]
    , focus :: Maybe X.Window
    }
    deriving stock (Show)

type WM = StateT WMState IO

io :: IO a -> WM a
io = lift

findManaged :: X.Window -> [ManagedWindow] -> Maybe ManagedWindow
findManaged w = List.find (\managed -> managed.window == w)

unmanage :: X.Window -> WMState -> WMState
unmanage w state =
    let windows = List.filter (\managed -> managed.window /= w) state.windows
        focus = fmap (.window) $ headMay windows
     in state{windows, focus}

defaultWindowChanges :: X.WindowChanges
defaultWindowChanges = X.WindowChanges 0 0 0 0 0 X.none 0

manage :: X.Display -> X.Window -> Int -> IO ManagedWindow
manage dpy window n = do
    let screenWidth = X.displayWidth dpy (X.defaultScreen dpy)
    let newWidth :: Float = realToFrac screenWidth / realToFrac n
    let newHeight = X.displayHeight dpy (X.defaultScreen dpy)
    let wc =
            defaultWindowChanges
                { X.wc_x = 0
                , X.wc_y = 0
                , X.wc_width = floor newWidth
                , X.wc_height = fromIntegral newHeight
                }
    let mask = X.cWX .|. X.cWY .|. X.cWWidth .|. X.cWHeight
    X.selectInput dpy window X.structureNotifyMask
    X.configureWindow dpy window (fromIntegral mask) wc
    pure $ ManagedWindow window (floor newWidth, fromIntegral newHeight)

resizeWindow :: X.Display -> X.Window -> (Int, Int) -> IO ()
resizeWindow dpy w (nth, n) = do
    let height = X.displayHeight dpy (X.defaultScreen dpy)
    let screenWidth = X.displayWidth dpy (X.defaultScreen dpy)
    let width :: Float = realToFrac screenWidth / realToFrac n
    let x = nth * floor width
    X.moveResizeWindow dpy w (fromIntegral x) 0 (floor width) (fromIntegral height)

notifyConfigure :: X.Display -> ManagedWindow -> IO ()
notifyConfigure dpy managed = X.allocaXEvent $ \p -> do
    X.setEventType p X.configureNotify
    let (w, h) = managed.dimensions
    X.setConfigureEvent
        p
        managed.window
        managed.window
        0
        0
        (fromIntegral w)
        (fromIntegral h)
        0
        X.none
        False
    X.sendEvent dpy managed.window False X.structureNotifyMask p

updateFocus :: X.Display -> WMState -> IO ()
updateFocus dpy state = do
    let target = Maybe.fromMaybe X.none state.focus
    forM_ state.windows $ \m -> do
        when (Just m.window /= state.focus) $
            X.grabButton
                dpy
                X.button1
                X.anyModifier
                m.window
                False
                X.buttonPressMask
                X.grabModeAsync
                X.grabModeSync
                X.none
                X.none
    traverse_ (X.ungrabButton dpy X.button1 X.anyModifier) state.focus
    X.setInputFocus dpy target X.revertToNone X.currentTime

handleCommand :: X.Display -> WMState -> Command -> IO WMState
handleCommand dpy state cmd = do
    let newState = Maybe.fromJust $
            case cmd of
                FocusRight -> do
                    focus <- state.focus
                    index <- List.findIndex (\m -> m.window == focus) state.windows
                    let newIndex = min (length state.windows - 1) (index + 1)
                    pure state{focus = Just (state.windows List.!! newIndex).window}
                FocusLeft -> do
                    focus <- state.focus
                    index <- List.findIndex (\m -> m.window == focus) state.windows
                    let newIndex = max 0 (index - 1)
                    pure state{focus = Just (state.windows List.!! newIndex).window}
    updateFocus dpy newState
    pure newState

addWindow :: X.Display -> X.Window -> WM ManagedWindow
addWindow dpy w = do
    managedWindows <- State.gets (.windows)
    case findManaged w managedWindows of
        Nothing -> do
            let n = length managedWindows + 1
            -- FIXME: manage -> WM
            managed <- io $ manage dpy w n
            let windows = managed : managedWindows
            forM_ (zip [1 ..] managedWindows) $ \(i, m) -> do
                io $ resizeWindow dpy m.window (i, n)
            State.modify' $ \s -> s{windows}
            pure managed
        Just m -> pure m

eventLoop :: X.Display -> X.XEventPtr -> WM ()
eventLoop dpy pevt = go
  where
    go = forever $ do
        io $ X.sync dpy False
        io $ X.nextEvent dpy pevt
        state <- State.get
        evt <- io $ X.getEvent pevt
        case evt of
            X.MapRequestEvent{ev_window = w} -> do
                void $ addWindow dpy w
                io $ X.mapWindow dpy w
                State.modify' $ \s -> s{focus = Just w}
                newState <- State.get
                io $ updateFocus dpy newState
            X.ConfigureRequestEvent{ev_window = w} -> do
                managed <- addWindow dpy w
                io $ notifyConfigure dpy managed
            X.DestroyWindowEvent{ev_window = window} -> do
                let newState = unmanage window state
                forM_ (zip [0 ..] newState.windows) $ \(i, m) -> do
                    io $ resizeWindow dpy m.window (i, length newState.windows)
                io $ updateFocus dpy newState
                State.put newState
            X.ClientMessageEvent{ev_message_type = t, ev_data = bytes} -> do
                when (t == 127) $ do
                    io $ putStrLn "Received client message:"
                    io $ print evt
                    case decode bytes of
                        Just cmd -> do
                            io $ putStrLn $ "Received command: " <> show cmd
                            newState <- io $ handleCommand dpy state cmd
                            State.put newState
                        Nothing -> pure ()
            X.ButtonEvent{ev_window = window} -> do
                newState <- pure state{focus = Just window}
                io $ updateFocus dpy newState
                State.put newState
            _ -> io $ putStrLn "  got unhandled event"

main :: IO ()
main = do
    dpy <- X.openDisplay ":1"
    X.selectInput dpy (X.defaultRootWindow dpy) X.substructureRedirectMask
    X.allocaXEvent $ \pevt ->
        void $ runStateT (eventLoop dpy pevt) (WMState [] Nothing)
    X.closeDisplay dpy
