module Maw.Command where

import Data.Word (Word8)
import Foreign.C (CInt)
import GHC.Generics (Generic)

data Command
    = FocusLeft
    | FocusRight
    deriving stock (Show, Eq, Enum, Generic)

newtype ByteMessage = ByteMessage {values :: [Word8]}
    deriving stock (Show, Eq)

mkByteMessage :: [Word8] -> ByteMessage
mkByteMessage = ByteMessage . take 20

encode :: Command -> ByteMessage
encode cmd = mkByteMessage [fromIntegral $ fromEnum cmd]

decode :: [CInt] -> Maybe Command
decode = \case
    b : _ -> pure . toEnum $ fromIntegral b
    _ -> Nothing
