{-# language RecordWildCards #-}

module Prometheus.Push.PushGateway where

import Data.ByteString (ByteString)


-- | The configuration of a push gateway.

data PushGateway = PushGateway
  { -- | A record accessor to get/set the host of the push gateway.
    pushGatewayHost :: !ByteString
  , -- | A record accessor to get/set the port of the push gateway.
    pushGatewayPort :: !Int
  }


  
-- | Create a new 'PushGateway' configuration to a particular host.
-- 'pushGatewayPort' will default to 9091.

pushGateway :: ByteString -> PushGateway
pushGateway pushGatewayHost =
  PushGateway{ pushGatewayPort = 9091,  .. }
