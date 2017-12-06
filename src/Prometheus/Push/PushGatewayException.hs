module Prometheus.Push.PushGatewayException where

import Control.Exception
import Data.Text
import Network.HTTP.Types.Status


-- | Errors encountered while trying to communicate with a push gateway server.

data PushGatewayException =

  -- | A request could not be processed by the push gateway server. Thrown on
  -- @4xx@ or @5xx@ responses.

  PushGatewayException
    { pushGatewayStatus :: !Status
    , pushGatewayError :: !Text
    }

  deriving (Eq, Show)


instance Exception PushGatewayException
