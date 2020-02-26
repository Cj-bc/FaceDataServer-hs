{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module FaceDataServer.Types where

import Control.Lens (makeLenses, (^.))
import Control.Monad (when)
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (fromStrict, toStrict)


(protocolMajorNum, protocolMinorNum) = (1, 0)

type Radian = Double
type Percent = Int

-- | Represent 'FaceData' which will be served from the server
data FaceData = FaceData { _face_x_radian :: Radian
                         , _face_y_radian :: Radian
                         , _face_z_radian :: Radian
                         , _mouth_height_percent :: Percent
                         , _mouth_width_percent  :: Percent
                         , _left_eye_percent     :: Percent
                         , _right_eye_percent    :: Percent
                         }
makeLenses ''FaceData

-- | Default value for FaceData
defaultFaceData = FaceData 0.0 0.0 0.0 0 0 0 0

-- instance Binary FaceData {{{
instance Binary FaceData where
    put fd = do
        let encodeNum        = toStrict . encode
            encodeVersionNum = BS.drop 4 . encodeNum
        putByteString $ encodeVersionNum protocolMajorNum
        putByteString $ encodeVersionNum protocolMinorNum

        putDoublebe $ fd^.face_x_radian
        putDoublebe $ fd^.face_y_radian
        putDoublebe $ fd^.face_z_radian
        putByteString . encodeNum $ fd^.mouth_height_percent
        putByteString . encodeNum $ fd^.mouth_width_percent
        putByteString . encodeNum $ fd^.left_eye_percent
        putByteString . encodeNum $ fd^.right_eye_percent

    get = do
        let null4bit = "\NUL\NUL\NUL\NUL" :: BS.ByteString
            decodeNum = decode . fromStrict

        -- == Eat version number
        vMajor <- getByteString 4
        let vMajorInt  = decodeNum (null4bit <> vMajor) :: Int
        when (vMajorInt /= protocolMajorNum) $ error "Major version not match"

        vMinor <- getByteString 4
        let vMinorInt = decodeNum (null4bit <> vMinor) :: Int
        when (vMinorInt /= protocolMinorNum) $ error "Minor version not match"

        -- == Eat actual data
        FaceData <$> getDoublebe
                 <*> getDoublebe
                 <*> getDoublebe
                 <*> (decodeNum <$> getByteString 8)
                 <*> (decodeNum <$> getByteString 8)
                 <*> (decodeNum <$> getByteString 8)
                 <*> (decodeNum <$> getByteString 8)
-- }}}
