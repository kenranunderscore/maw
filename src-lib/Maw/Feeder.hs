module Maw.Feeder where

import Graphics.X11 qualified as X
import Graphics.X11.Xlib.Extras qualified as X

import Maw.Command

sendClientMessage :: X.Display -> ByteMessage -> IO ()
sendClientMessage dpy msg = X.allocaXEvent $ \pevt -> do
    let root = X.defaultRootWindow dpy
    let payload = fmap fromIntegral msg.values
    X.setEventType pevt X.clientMessage
    X.setClientMessageEvent' pevt root 127 8 payload
    X.sendEvent dpy root False X.substructureRedirectMask pevt
    X.flush dpy

sendCommand :: X.Display -> Command -> IO ()
sendCommand dpy = sendClientMessage dpy . encode

main :: IO ()
main = do
    putStrLn "Feeder reporting for duty"
    dpy <- X.openDisplay ":1"
    sendCommand dpy FocusRight
    X.closeDisplay dpy
