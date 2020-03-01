{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module FaceDataServer.Types where

import Control.Lens (makeLenses, (^.))
import Control.Monad (when)
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Int (Int8)


protocolMajorNum, protocolMinorNum :: Int8
(protocolMajorNum, protocolMinorNum) = (1, 0)

type Radian = Double
type Percent = Int8

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
        putInt8 $ shift protocolMajorNum 4 + protocolMinorNum

        putDoublebe $ fd^.face_x_radian
        putDoublebe $ fd^.face_y_radian
        putDoublebe $ fd^.face_z_radian
        putInt8 $ fd^.mouth_height_percent
        putInt8 $ fd^.mouth_width_percent
        putInt8 $ fd^.left_eye_percent
        putInt8 $ fd^.right_eye_percent

    get = do
        header <- getInt8
        let versionMaj = shift header (-4)
            -- Clear upper 4 bit by xor 0b11110000
            -- (As Int8 is singed, -112)
            versionMin = header `xor` (-112)

        when (versionMaj /= protocolMajorNum) $ error "Protocol Major version differ"
        when (versionMin /= protocolMinorNum) $ error "Protocol Minor version differ"

        -- == Eat actual data
        FaceData <$> getDoublebe
                 <*> getDoublebe
                 <*> getDoublebe
                 <*> getInt8
                 <*> getInt8
                 <*> getInt8
                 <*> getInt8
-- }}}
