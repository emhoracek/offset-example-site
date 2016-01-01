{-# LANGUAGE OverloadedStrings #-}

module Site where

import           Configuration.Dotenv  (loadFile)
import           Control.Lens          (use)
import           Control.Logging       (errorL')
import           Control.Monad         (when)
import qualified Data.ByteString.Char8 as BS (pack)
import           Data.Default          (def)
import           Data.Maybe            (fromMaybe)
import           Data.Monoid           ((<>))
import           Data.Text             (Text, intercalate, pack, unpack)
import           Data.Text.Encoding    (decodeUtf8)
import qualified Database.Redis        as R
import           Network.Wai           (Application, Response, rawPathInfo)
import           System.Directory      (doesFileExist)
import           System.Environment    (lookupEnv)


import           Web.Fn
import           Web.Fn.Extra.Heist
import           Web.Offset

import           Context

site :: Ctxt -> IO Response
site ctxt =
  route ctxt [ end ==> homeHandler
             , path "blog" ==> blogHandler
             , path "heist" ==> heistServe
             , path "static" ==> staticServe "static" ]
  `fallthrough` notFoundText "Not found."

wpConf = WordpressConfig "http://127.0.0.1:5555/wp-json" (Left ("offset", "111")) (CacheSeconds 600) [] Nothing

homeHandler :: Ctxt -> IO (Maybe Response)
homeHandler ctxt = render ctxt "home"

blogHandler :: Ctxt -> IO (Maybe Response)
blogHandler ctxt = render ctxt "post"

-- One big honkin' function O_o
initializer :: IO Ctxt
initializer = do

  -- Load environment variables, or use defaults
  let lookupWithDefault key def = pack <$> fromMaybe def <$> lookupEnv key
  envExists <- doesFileExist ".env"
  when envExists $ loadFile False ".env"

  -- redis env variables
  cRServer <- lookupWithDefault "REDIS_SERVER" "localhost"
  cRAuth <- (fmap.fmap) BS.pack (lookupEnv "REDIS_AUTH")

  -- get redis connection information
  rconn <- R.connect $ R.defaultConnectInfo { R.connectHost = unpack cRServer, R.connectAuth = cRAuth }

  -- wordpress env variables
  cWpServer <- lookupWithDefault "WP_SERVER" "http://127.0.0.1:5555"
  cWpUser <- lookupWithDefault "WP_USER" "offset"
  cWpPass <- lookupWithDefault "WP_PASS" "111"

  -- Set up wordpress configuration
  let wpconf = def { wpConfEndpoint = cWpServer <> "/wp-json"
                   , wpConfLogger = Just (putStrLn . unpack)
                   , wpConfRequester = Left (cWpUser, cWpPass)
                   , wpConfCacheBehavior = CacheSeconds 60 }

  -- get a connection to wordpress and wordpress splices for templates
  let rqURI = decodeUtf8 . rawPathInfo <$> fst <$> use requestLens
  (wp, wpSplices) <- initWordpress wpconf rconn rqURI wordpress

  -- set up heist with the wordpress splices
  hs' <- heistInit ["templates"] mempty wpSplices
  let hs = case hs' of
        Left errs ->
          errorL' ("Heist failed to load templates: \n" <> intercalate "\n" (map pack errs))
        Right  hs'' -> hs''

  -- return a Ctxt from which we can get all this stuff!
  return (Ctxt defaultFnRequest rconn wp hs)

app :: IO Application
app = do
  ctxt <- initializer
  return $ toWAI ctxt site
