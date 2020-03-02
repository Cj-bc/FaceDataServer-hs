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


protocolMajorNum, protocolMinorNum :: Int
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
defaultFaceData = FaceData 0.0 0.0 0.0 100 100 100 100

-- instance Binary FaceData {{{
instance Binary FaceData where
    put fd = do
        putInt8 $ fromIntegral $ shift protocolMajorNum 4 + protocolMinorNum

        putDoublebe $ fd^.face_x_radian
        putDoublebe $ fd^.face_y_radian
        putDoublebe $ fd^.face_z_radian
        putInt8 $ fromIntegral $ fd^.mouth_height_percent
        putInt8 $ fromIntegral $ fd^.mouth_width_percent
        putInt8 $ fromIntegral $ fd^.left_eye_percent
        putInt8 $ fromIntegral $ fd^.right_eye_percent

    get = do
        header <- getInt8
        let versionMaj = fromIntegral $ shift header (-4)
            -- Clear upper 4 bit by xor 0b11110000
            -- (As Int8 is singed, -112)
            versionMin = fromIntegral $ header `xor` (-112)

        when (versionMaj /= protocolMajorNum) $ error $ unlines [ "Protocol Major version differ:"
                                                                , "protocol used in this lib: " ++ show protocolMajorNum
                                                                , "protocol used by peer: " ++ show versionMaj
                                                                ]
        when (versionMin /= protocolMinorNum) $ error $ unlines [ "Protocol Minor version differ:"
                                                                , "protocol used in this lib: " ++ show protocolMinorNum
                                                                , "protocol used by peer: " ++ show versionMin
                                                                ]

        -- == Eat actual data
        FaceData <$> getDoublebe
                 <*> getDoublebe
                 <*> getDoublebe
                 <*> (fromIntegral <$> getInt8)
                 <*> (fromIntegral <$> getInt8)
                 <*> (fromIntegral <$> getInt8)
                 <*> (fromIntegral <$> getInt8)
-- }}}
