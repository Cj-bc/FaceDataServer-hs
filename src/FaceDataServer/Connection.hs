{-|
Module      : FaceDataServer.Connection
Description : FaceDataServer
Copyright   : (c) Cj.bc_sd a.k.a Cj-bc, 2020
Maintainer  : cj.bc-sd@outlook.jp
Stability   : experimental


-}
module FaceDataServer.Connection where

import Data.Binary (decode, encode)
import Data.ByteString.Lazy (fromStrict, toStrict)
import FaceDataServer.Types
import Network.Socket (Socket, SockAddr)
import Network.Socket.ByteString (recv, sendTo)


getFaceData :: Socket -> IO FaceData
getFaceData sock = decode . fromStrict <$> recv sock (8 * 29)

sendFaceData :: Socket -> SockAddr -> FaceData -> IO Int
sendFaceData sock addr d = do
    let encoded = toStrict $ encode d
    sendTo sock encoded addr
