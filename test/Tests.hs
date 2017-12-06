{-# language OverloadedStrings #-}

module Tests where

import Prometheus.Push

import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
import Data.Monoid
import qualified Data.Text.Encoding as T
import Network.HTTP.Client
import qualified Network.HTTP.Client as HTTP 
import Network.HTTP.Client.Internal (Response(..), ResponseClose(..))
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status
import Network.HTTP.Types.Version
import Prometheus
import Test.Tasty.Hspec

spec_requests :: Spec
spec_requests =
  before_ setupTestMetrics $
  describe "Request generation" $ do
    it "Generates valid POST requests" $ do
      expected <- exportMetricsAsText

      let req = postMetricsRequest (pushGateway "pushgateway.example") (job "test_job")

      HTTP.host req `shouldBe` "pushgateway.example"
      HTTP.method req `shouldBe` methodPost
      HTTP.path req `shouldBe` C8.pack "/metrics/job/test_job"
      HTTP.secure req `shouldBe` False
      HTTP.port req `shouldBe` 9091
      HTTP.queryString req `shouldBe` mempty
      drainRequestBody (HTTP.requestBody req) `shouldReturn` expected

    it "Generates valid PUT requests" $ do
      expected <- exportMetricsAsText

      let req = putMetricsRequest (pushGateway "pushgateway.example") (job "test_job")

      HTTP.host req `shouldBe` "pushgateway.example"
      HTTP.method req `shouldBe` methodPut
      HTTP.path req `shouldBe` C8.pack "/metrics/job/test_job"
      HTTP.secure req `shouldBe` False
      HTTP.port req `shouldBe` 9091
      HTTP.queryString req `shouldBe` mempty
      drainRequestBody (HTTP.requestBody req) `shouldReturn` expected

    it "Generates valid DELETE requests" $ do
      let req = deleteMetricsRequest (pushGateway "pushgateway.example") (job "test_job")

      HTTP.host req `shouldBe` "pushgateway.example"
      HTTP.method req `shouldBe` methodDelete
      HTTP.path req `shouldBe` C8.pack "/metrics/job/test_job"
      HTTP.secure req `shouldBe` False
      HTTP.port req `shouldBe` 9091
      HTTP.queryString req `shouldBe` mempty
      drainRequestBody (HTTP.requestBody req) `shouldReturn` mempty


spec_sendRequest :: Spec
spec_sendRequest = do
  it "Returns successfully on 2xx responses" $
    sendPushGatewayRequest
      ( \req ->
          return
            Response
              { responseStatus = ok200
              , responseVersion = http11
              , responseHeaders = []
              , responseBody = mempty
              , responseCookieJar = mempty
              , responseClose' = ResponseClose (return ())
              } )
      (postMetricsRequest (pushGateway "pushgateway.example") (job "test_job"))
      `shouldReturn` ()

  it "Throws on 4xx responses" $ do
    let
      status = badRequest400
      msg = "Error message"

      expected =
        PushGatewayException
          { pushGatewayStatus = status
          , pushGatewayError = msg
          }

    sendPushGatewayRequest
      ( \req ->
          return
            Response
              { responseStatus = status
              , responseVersion = http11
              , responseHeaders = []
              , responseBody = LBS.fromStrict (T.encodeUtf8 msg)
              , responseCookieJar = mempty
              , responseClose' = ResponseClose (return ())
              } )
      ( postMetricsRequest (pushGateway "pushgateway.example") (job "test_job") )
      `shouldThrow` (== expected)


drainRequestBody :: RequestBody -> IO ByteString
drainRequestBody (RequestBodyLBS lbs) = return (LBS.toStrict lbs)
drainRequestBody (RequestBodyBS bs) = return bs
drainRequestBody (RequestBodyBuilder _ builder) = return (LBS.toStrict (Builder.toLazyByteString builder))
drainRequestBody (RequestBodyStream _ givesPopper) = do
  res <- newIORef mempty
  givesPopper (\popper  -> popper >>= writeIORef res)
  readIORef res
drainRequestBody (RequestBodyStreamChunked givesPopper) = do
  res <- newIORef mempty
  givesPopper (\popper  -> popper >>= writeIORef res)
  readIORef res
drainRequestBody (RequestBodyIO io) =
  io >>= drainRequestBody


setupTestMetrics :: IO ()
setupTestMetrics = do
  registerIO $ counter (Info "test_counter" "Test Counter")
  return ()
