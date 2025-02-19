module Maw.Command where

import Data.Word (Word8)

data Command
    = MoveLeft
    | MoveRight
    deriving stock (Show)

newtype ByteMessage = ByteMessage {values :: [Word8]}
    deriving stock (Show)

mkByteMessage :: [Word8] -> ByteMessage
mkByteMessage = ByteMessage . take 20

encode :: Command -> ByteMessage
encode =
    mkByteMessage . (\x -> [x]) . \case
        MoveLeft -> 1
        MoveRight -> 2
