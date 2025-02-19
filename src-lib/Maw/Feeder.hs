module Maw.Feeder where

import Graphics.X11 qualified as X
import Graphics.X11.Xlib.Extras qualified as X

main :: IO ()
main = do
    putStrLn "Feeder reporting for duty"
    dpy <- X.openDisplay ":1"
    let root = X.defaultRootWindow dpy
    X.allocaXEvent $ \pevt -> do
        let payload = [1 .. 20]
        X.setEventType pevt X.clientMessage
        X.setClientMessageEvent' pevt root 127 8 payload
        X.sendEvent dpy root False X.substructureRedirectMask pevt
        X.flush dpy
