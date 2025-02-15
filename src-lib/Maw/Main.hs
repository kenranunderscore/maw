module Maw.Main where

import Data.Bits ((.|.))
import Graphics.X11 qualified as X
import Graphics.X11.Xlib.Extras (Event (ev_border_width))
import Graphics.X11.Xlib.Extras qualified as X

eventLoop :: X.Display -> Int -> IO ()
eventLoop dpy nwins = X.allocaXEvent $ \pe -> do
    X.nextEvent dpy pe
    evt <- X.getEvent pe
    print evt
    case evt of
        X.MapRequestEvent{ev_window = w, ev_parent = _p} -> do
            putStrLn "map requested"
            X.mapWindow dpy w
            X.sync dpy False
            X.flush dpy
            eventLoop dpy nwins
        X.ConfigureRequestEvent
            { ev_window = w
            , ev_value_mask = valueMask
            , ev_border_width = bw
            , ev_above = above
            , ev_detail = detail
            } -> do
                putStrLn "configure requested"
                let height = X.displayHeight dpy (X.defaultScreen dpy)
                let screenWidth = X.displayWidth dpy (X.defaultScreen dpy)
                let n = nwins + 1
                let width :: Float = realToFrac screenWidth / realToFrac n
                let wc = X.WindowChanges 0 0 (floor width) (fromIntegral height) bw above detail
                X.moveResizeWindow dpy w 0 0 (floor width) (fromIntegral height)
                X.configureWindow dpy w valueMask wc
                X.sync dpy False
                X.flush dpy
                eventLoop dpy n
        _ -> do
            t <- X.get_EventType pe
            print $ "unhandled event: " <> show t

main :: IO ()
main = do
    dpy <- X.openDisplay ":1"
    X.selectInput dpy (X.defaultRootWindow dpy) X.substructureRedirectMask
    X.sync dpy False
    eventLoop dpy 0
    X.closeDisplay dpy
