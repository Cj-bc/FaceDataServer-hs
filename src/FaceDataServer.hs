{-|
Module      : FaceDataServer
Description : Implementation of FDS Protocol
Copyright   : (c) Cj.bc_sd a.k.a Cj-bc, 2020
Maintainer  : cj.bc-sd@outlook.jp
Stability   : experimental

See: https://github.com/Cj-bc/FDS-proto
Current protocol version: 1.0.0
-}

module FaceDataServer
( FaceData
, defaultGroupAddr
, defaultPortNumber
) where

import FaceDataServer.Types
import Network.Socket (PortNumber)

defaultGroupAddr = "226.70.68.83"
defaultPortNumber = 5032 :: PortNumber
