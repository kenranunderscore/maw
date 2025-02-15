module Maw.Main where

import Control.Monad (forM_)
import Data.Bits ((.|.))
import Data.List qualified as List
import Graphics.X11 qualified as X
import Graphics.X11.Xlib.Extras qualified as X

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

eventLoop :: X.Display -> [X.Window] -> IO ()
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
            if window `elem` managedWindows
                then do
                    putStrLn "  known window, doing nothing"
                    X.allocaXEvent $ \p -> do
                        X.setEventType p X.configureNotify
                        X.setConfigureEvent p window window 0 0 100 150 5 X.none False
                        X.sendEvent dpy window False X.structureNotifyMask p
                    eventLoop dpy managedWindows
                else do
                    putStrLn "  newly managed window appeared!"
                    let screenWidth = X.displayWidth dpy (X.defaultScreen dpy)
                    let windows = window : managedWindows
                    let newWidth :: Float = realToFrac screenWidth / realToFrac (length windows)
                    let wc =
                            defaultWindowChanges
                                { X.wc_x = 0
                                , X.wc_y = 0
                                , X.wc_width = floor newWidth
                                , X.wc_height = fromIntegral $ X.displayHeight dpy (X.defaultScreen dpy)
                                }
                    let mask = X.cWX .|. X.cWY .|. X.cWWidth .|. X.cWHeight
                    X.configureWindow dpy window (fromIntegral mask) wc
                    X.selectInput dpy window X.structureNotifyMask
                    forM_ (zip [1 ..] managedWindows) $ \(i, w) -> do
                        placeWindow dpy w (i, length windows)
                    eventLoop dpy windows
        X.DestroyWindowEvent{ev_window = window} -> do
            putStrLn $ "  window destroyed: " <> show window
            let windows = List.delete window managedWindows
            forM_ (zip [0 ..] windows) $ \(i, w) -> do
                placeWindow dpy w (i, length windows)
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
