module Maw.Main where

import Control.Monad (forM_)
import Data.Bits ((.|.))
import Data.List qualified as List
import Graphics.X11 qualified as X
import Graphics.X11.Xlib.Extras qualified as X

data ManagedWindow = ManagedWindow
    { window :: X.Window
    , dimensions :: (Int, Int)
    }
    deriving stock (Show)

findManaged :: X.Window -> [ManagedWindow] -> Maybe ManagedWindow
findManaged w = List.find (\managed -> managed.window == w)

unmanage :: X.Window -> [ManagedWindow] -> [ManagedWindow]
unmanage w = List.filter (\managed -> managed.window /= w)

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
    X.configureWindow dpy window (fromIntegral mask) wc
    X.selectInput dpy window X.structureNotifyMask
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

eventLoop :: X.Display -> IO ()
eventLoop dpy = X.allocaXEvent $ \pevt ->
    let go managedWindows = do
            X.sync dpy False
            X.nextEvent dpy pevt
            evt <- X.getEvent pevt
            print evt
            case evt of
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
                    go windows
                X.ConfigureRequestEvent{ev_window = window} -> do
                    case findManaged window managedWindows of
                        Nothing -> do
                            let n = length managedWindows + 1
                            managed <- addWindow dpy window n
                            let windows = managed : managedWindows
                            forM_ (zip [1 ..] managedWindows) $ \(i, m) -> do
                                resizeWindow dpy m.window (i, n)
                            go windows
                        Just managed -> do
                            notifyConfigure dpy managed
                            go managedWindows
                X.DestroyWindowEvent{ev_window = window} -> do
                    let windows = unmanage window managedWindows
                    forM_ (zip [0 ..] windows) $ \(i, m) -> do
                        resizeWindow dpy m.window (i, length windows)
                    go windows
                _ -> go managedWindows
     in go []

main :: IO ()
main = do
    dpy <- X.openDisplay ":1"
    X.selectInput dpy (X.defaultRootWindow dpy) X.substructureRedirectMask
    eventLoop dpy
    X.closeDisplay dpy
