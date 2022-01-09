{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}

module Ratel
  ( notify
  , toError
  , toTraces
  , toTrace
  , ApiKey
  , Payload(..)
  , Error(..)
  , Notifier(..)
  , Request(..)
  , Server(..)
  , Trace(..)
  , Project(..)
  , Notice(..)
  , NoticeUuid(..)
  ) where

import qualified Control.Exception as Exception
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import qualified Data.ByteString.Char8 as BS
import qualified Data.CaseInsensitive as CI
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Typeable as Typeable
import qualified Data.UUID as UUID
import qualified Data.Version as Version
import qualified GHC.Stack as Stack
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.TLS as Client
import qualified Network.HTTP.Types as HTTP
import qualified Paths_ratel as This


notify :: ApiKey -> Maybe Client.Manager -> Payload -> IO UUID.UUID
notify apiKey maybeManager initialPayload = do
  let
    notifier = Notifier
      { notifierName = Just "Ratel"
      , notifierUrl = Just "https://github.com/tfausak/ratel"
      , notifierVersion = Just (Version.showVersion This.version)
      }
  let
    payload = case payloadNotifier initialPayload of
      Nothing -> initialPayload { payloadNotifier = Just notifier }
      _ -> initialPayload

  initialRequest <- Client.parseUrlThrow
    "https://api.honeybadger.io/v1/notices"
  let body = Client.RequestBodyLBS (JSON.encode payload)
  let
    headers =
      [ (HTTP.hAccept, BS.pack "application/json")
      , (HTTP.hContentType, BS.pack "application/json")
      , (CI.mk (BS.pack "X-API-Key"), BS.pack apiKey)
      ]
  let
    request = initialRequest
      { Client.method = BS.pack "POST"
      , Client.requestBody = body
      , Client.requestHeaders = headers
      }

  manager <- case maybeManager of
    Nothing -> Client.newManager Client.tlsManagerSettings
    Just manager -> return (Client.getHttpManager manager)

  response <- Client.httpLbs request manager
  case JSON.eitherDecode (Client.responseBody response) of
    Left message -> fail message
    Right notice -> return (unwrapNoticeUuid (noticeUuid notice))


toError
  :: (Exception.Exception exception, Stack.HasCallStack) => exception -> Error
toError exception = Error
  { errorBacktrace = Just (toTraces ?callStack)
  , errorClass = Just (show (Typeable.typeOf exception))
  , errorMessage = Just (Exception.displayException exception)
  , errorSource = Nothing
  , errorTags = Nothing
  }


toTraces :: Stack.CallStack -> [Trace]
toTraces callStack = fmap (uncurry toTrace) (Stack.getCallStack callStack)


toTrace :: String -> Stack.SrcLoc -> Trace
toTrace function srcLoc = Trace
  { traceFile = Just (Stack.srcLocFile srcLoc)
  , traceMethod = Just
    (List.intercalate "." [Stack.srcLocModule srcLoc, function])
  , traceNumber = Just
    (List.intercalate
      ":"
      (fmap show [Stack.srcLocStartLine srcLoc, Stack.srcLocStartCol srcLoc])
    )
  }


type ApiKey = String


data Payload = Payload
  { payloadError :: Error
  , payloadNotifier :: Maybe Notifier
  , payloadRequest :: Maybe Request
  , payloadServer :: Server
  }
  deriving (Eq, Show)

instance JSON.ToJSON Payload where
  toJSON x = JSON.object
    [ "error" JSON..= payloadError x
    , "notifier" JSON..= payloadNotifier x
    , "request" JSON..= payloadRequest x
    , "server" JSON..= payloadServer x
    ]


data Error = Error
  { errorBacktrace :: Maybe [Trace]
  , errorClass :: Maybe String
  , errorMessage :: Maybe String
  , errorSource :: Maybe (Map.Map String String)
  , errorTags :: Maybe [String]
  }
  deriving (Eq, Show)

instance JSON.ToJSON Error where
  toJSON x = JSON.object
    [ "backtrace" JSON..= errorBacktrace x
    , "class" JSON..= errorClass x
    , "message" JSON..= errorMessage x
    , "source" JSON..= errorSource x
    , "tags" JSON..= errorTags x
    ]


data Notifier = Notifier
  { notifierName :: Maybe String
  , notifierUrl :: Maybe String
  , notifierVersion :: Maybe String
  }
  deriving (Eq, Show)

instance JSON.ToJSON Notifier where
  toJSON x = JSON.object
    [ "name" JSON..= notifierName x
    , "url" JSON..= notifierUrl x
    , "version" JSON..= notifierVersion x
    ]


data Request = Request
  { requestAction :: Maybe String
  , requestCgiData :: Maybe (Map.Map String String)
  , requestComponent :: Maybe String
  , requestContext :: Maybe (Map.Map String JSON.Value)
  , requestParams :: Maybe (Map.Map String String)
  , requestSession :: Maybe (Map.Map String String)
  , requestUrl :: Maybe String
  }
  deriving (Eq, Show)

instance JSON.ToJSON Request where
  toJSON x = JSON.object
    [ "action" JSON..= requestAction x
    , "cgi_data" JSON..= requestCgiData x
    , "component" JSON..= requestComponent x
    , "context" JSON..= requestContext x
    , "params" JSON..= requestParams x
    , "session" JSON..= requestSession x
    , "url" JSON..= requestUrl x
    ]


data Server = Server
  { serverEnvironmentName :: Maybe String
  , serverHostname :: Maybe String
  , serverProjectRoot :: Maybe Project
  }
  deriving (Eq, Show)

instance JSON.ToJSON Server where
  toJSON x = JSON.object
    [ "environment_name" JSON..= serverEnvironmentName x
    , "hostname" JSON..= serverHostname x
    , "project_root" JSON..= serverProjectRoot x
    ]


data Trace = Trace
  { traceFile :: Maybe String
  , traceMethod :: Maybe String
  , traceNumber :: Maybe String
  }
  deriving (Eq, Show)

instance JSON.ToJSON Trace where
  toJSON x = JSON.object
    [ "file" JSON..= traceFile x
    , "method" JSON..= traceMethod x
    , "number" JSON..= traceNumber x
    ]


newtype Project = Project
    { projectPath :: Maybe String
    } deriving (Eq, Show)

instance JSON.ToJSON Project where
  toJSON x = JSON.object ["path" JSON..= projectPath x]


newtype Notice = Notice
    { noticeUuid :: NoticeUuid
    } deriving (Eq, Show)

instance JSON.FromJSON Notice where
  parseJSON json = case json of
    JSON.Object object -> do
      uuid <- object JSON..: "id"
      return Notice { noticeUuid = uuid }
    _ -> JSON.typeMismatch "Notice" json


newtype NoticeUuid = NoticeUuid
    { unwrapNoticeUuid :: UUID.UUID
    } deriving (Eq, Show)

instance JSON.FromJSON NoticeUuid where
  parseJSON json = case json of
    JSON.String text -> case UUID.fromText text of
      Nothing -> JSON.typeMismatch "UUID" json
      Just uuid -> return (NoticeUuid uuid)
    _ -> JSON.typeMismatch "UUID" json
