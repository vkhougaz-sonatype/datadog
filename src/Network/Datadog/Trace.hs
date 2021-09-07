{-# LANGUAGE OverloadedStrings #-}

{-|
Sends traces upstream
-}
module Network.Datadog.Trace
( sendTrace
) where

import Data.Aeson
import Data.Maybe (fromJust)
import qualified Network.HTTP.Types  as HTTP
import qualified Network.HTTP.Client as C
import Control.Monad (void)
import Network.Datadog.Internal

traceRequest :: String -> C.Request
traceRequest baseUrl = (fromJust $ C.parseUrlThrow baseUrl) { C.method = HTTP.methodPut
                            , C.path = "/v0.3/traces"
                            , C.requestHeaders = [("Content-Type", "application/json")]
                            }

sendTrace :: DatadogClient k -> String -> [Span] -> IO ()
sendTrace m agentUrl s = void $ C.httpNoBody req $ datadogClientManager m
  where
    req = (traceRequest agentUrl) { C.requestBody = C.RequestBodyLBS $ encode s }
