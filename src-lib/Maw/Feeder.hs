module Maw.Feeder where

import Data.Word (Word8)
import Graphics.X11 qualified as X
import Graphics.X11.Xlib.Extras qualified as X

newtype ByteMessage = ByteMessage {values :: [Word8]}
    deriving stock (Show)

mkByteMessage :: [Word8] -> ByteMessage
mkByteMessage = ByteMessage . take 20

sendClientMessage :: X.Display -> ByteMessage -> IO ()
sendClientMessage dpy msg = X.allocaXEvent $ \pevt -> do
    let root = X.defaultRootWindow dpy
    let payload = fmap fromIntegral msg.values
    X.setEventType pevt X.clientMessage
    X.setClientMessageEvent' pevt root 127 8 payload
    X.sendEvent dpy root False X.substructureRedirectMask pevt
    X.flush dpy

main :: IO ()
main = do
    putStrLn "Feeder reporting for duty"
    dpy <- X.openDisplay ":1"
    sendClientMessage dpy (mkByteMessage [230])
