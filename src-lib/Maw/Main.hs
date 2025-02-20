module Maw.Main where

import Control.Monad
import Data.Bits ((.|.))
import Data.List qualified as List
import Data.Maybe qualified as Maybe
import Graphics.X11 qualified as X
import Graphics.X11.Xlib.Extras qualified as X
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

findManaged :: X.Window -> [ManagedWindow] -> Maybe ManagedWindow
findManaged w = List.find (\managed -> managed.window == w)

unmanage :: X.Window -> WMState -> WMState
unmanage w state =
    let windows = List.filter (\managed -> managed.window /= w) state.windows
        focus = fmap (.window) $ headMay windows
     in state{windows, focus}

defaultWindowChanges :: X.WindowChanges
defaultWindowChanges = X.WindowChanges 0 0 0 0 0 X.none 0

addWindow :: X.Display -> X.Window -> Int -> IO ManagedWindow
addWindow dpy window n = do
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
    X.selectInput dpy window (X.structureNotifyMask .|. X.focusChangeMask)
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
    X.setConfigureEvent p managed.window managed.window 0 0 (fromIntegral w) (fromIntegral h) 0 X.none False
    X.sendEvent dpy managed.window False X.structureNotifyMask p

updateFocus :: X.Display -> WMState -> IO ()
updateFocus dpy state = do
    let target = Maybe.fromMaybe X.none state.focus
    X.setInputFocus dpy target X.revertToNone X.currentTime

eventLoop :: X.Display -> IO ()
eventLoop dpy = X.allocaXEvent $ \pevt ->
    let go state@WMState{windows = managedWindows} = do
            X.sync dpy False
            X.nextEvent dpy pevt
            evt <- X.getEvent pevt
            case evt of
                X.FocusChangeEvent{ev_event_type = t} -> do
                    when (t == X.focusIn) $ do
                        print evt
                    go state
                X.MapRequestEvent{ev_window = w, ev_parent = _p} -> do
                    windows <- case findManaged w managedWindows of
                        Nothing -> do
                            let n = length managedWindows + 1
                            managed <- addWindow dpy w n
                            forM_ (zip [1 ..] managedWindows) $ \(i, m) -> do
                                resizeWindow dpy m.window (i, n)
                            pure $ managed : managedWindows
                        Just _ -> pure managedWindows
                    X.mapWindow dpy w
                    let newState = state{windows, focus = Just w}
                    updateFocus dpy newState
                    go newState
                X.ConfigureRequestEvent{ev_window = window} -> do
                    case findManaged window managedWindows of
                        Nothing -> do
                            let n = length managedWindows + 1
                            managed <- addWindow dpy window n
                            let windows = managed : managedWindows
                            forM_ (zip [1 ..] managedWindows) $ \(i, m) -> do
                                resizeWindow dpy m.window (i, n)
                            go state{windows}
                        Just managed -> do
                            notifyConfigure dpy managed
                            go state
                X.DestroyWindowEvent{ev_window = window} -> do
                    let newState = unmanage window state
                    forM_ (zip [0 ..] newState.windows) $ \(i, m) -> do
                        resizeWindow dpy m.window (i, length newState.windows)
                    updateFocus dpy newState
                    go newState
                X.ClientMessageEvent{} -> do
                    putStrLn "Received client message:"
                    print evt
                    go state
                _ -> do
                    putStrLn $ "    unhandled event: " <> show evt
                    go state
     in go (WMState [] Nothing)

main :: IO ()
main = do
    dpy <- X.openDisplay ":1"
    X.selectInput dpy (X.defaultRootWindow dpy) X.substructureRedirectMask
    eventLoop dpy
    X.closeDisplay dpy
