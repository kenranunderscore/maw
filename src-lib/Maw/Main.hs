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

placeWindow :: X.Display -> X.Window -> (Int, Int) -> IO ()
placeWindow dpy w (nth, n) = do
    let height = X.displayHeight dpy (X.defaultScreen dpy)
    let screenWidth = X.displayWidth dpy (X.defaultScreen dpy)
    let width :: Float = realToFrac screenWidth / realToFrac n
    let x = nth * floor width
    X.moveResizeWindow dpy w (fromIntegral x) 0 (floor width) (fromIntegral height)
    X.sync dpy False
    X.flush dpy

notifyConfigure :: X.Display -> ManagedWindow -> IO ()
notifyConfigure dpy managed = do
    X.allocaXEvent $ \p -> do
        X.setEventType p X.configureNotify
        let (w, h) = managed.dimensions
        X.setConfigureEvent p managed.window managed.window 0 0 (fromIntegral w) (fromIntegral h) 0 X.none False
        X.sendEvent dpy managed.window False X.structureNotifyMask p

eventLoop :: X.Display -> [ManagedWindow] -> IO ()
eventLoop dpy managedWindows = X.allocaXEvent $ \pe -> do
    X.sync dpy False
    X.flush dpy
    putStrLn "    ping"
    X.nextEvent dpy pe
    putStrLn "    pong"
    evt <- X.getEvent pe
    print evt
    case evt of
        X.MapRequestEvent{ev_window = w, ev_parent = _p} -> do
            X.mapWindow dpy w
            eventLoop dpy managedWindows
        X.ConfigureRequestEvent{ev_window = window} -> do
            case findManaged window managedWindows of
                Nothing -> do
                    putStrLn "  newly managed window appeared!"
                    let screenWidth = X.displayWidth dpy (X.defaultScreen dpy)
                    let newWidth :: Float = realToFrac screenWidth / realToFrac (length managedWindows + 1)
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
                    let managed = ManagedWindow window (floor newWidth, fromIntegral newHeight)
                    let windows = managed : managedWindows
                    forM_ (zip [1 ..] managedWindows) $ \(i, m) -> do
                        placeWindow dpy m.window (i, length windows)
                    eventLoop dpy windows
                Just managed -> do
                    putStrLn "  known window, just notifying"
                    notifyConfigure dpy managed
                    eventLoop dpy managedWindows
        X.DestroyWindowEvent{ev_window = window} -> do
            putStrLn $ "  window destroyed: " <> show window
            let windows = unmanage window managedWindows
            forM_ (zip [0 ..] windows) $ \(i, m) -> do
                placeWindow dpy m.window (i, length windows)
            eventLoop dpy windows
        _ -> do
            putStrLn " ..unhandled"
            eventLoop dpy managedWindows

main :: IO ()
main = do
    dpy <- X.openDisplay ":1"
    X.selectInput dpy (X.defaultRootWindow dpy) X.substructureRedirectMask
    eventLoop dpy mempty
    X.closeDisplay dpy
