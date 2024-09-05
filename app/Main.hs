{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NumDecimals #-}
module Main where

import Network.HTTP.Client.Internal qualified as Http
import Servant.Client
import Servant.API
import Servant.Server
import Servant.Links
import Data.Proxy
import GHC.Generics (Generic)
import Network.URI (uriToString, relativeTo, parseAbsoluteURI)
import Data.Function ((&))
import Network.Wai.Handler.Warp (run)
import Servant (throwError)
import Control.Monad.Reader (local)
import GHC.TypeLits (Symbol)
import Control.Exception (throwIO)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.Async
import Control.Concurrent (threadDelay)
import Data.Maybe (fromJust)

overrideUrl :: String -> ClientM a -> ClientM a
overrideUrl url action = do
    request <- Http.parseRequest url
    let transformClientRequest original = 
            original { Http.path = request.path, Http.queryString = request.queryString  }
        transformMakeClientRequest f baseUrl servantReq = do 
            httpReq <- f baseUrl servantReq 
            pure $ transformClientRequest httpReq
        transformClientEnv clientEnv = 
            clientEnv { makeClientRequest = transformMakeClientRequest clientEnv.makeClientRequest }
    local transformClientEnv action 

paginated :: 
    forall (s :: Symbol) rest a . Monoid a => 
    ClientM (Headers (Header s String ': rest) a) ->
    ClientM (Headers (Header s String ': rest) a)
paginated initial = do
    let go action acc = do
            r <- action
            let acc' = acc <> getResponse r
                HCons header _ = getHeadersHList r
            case header of 
                UndecodableHeader {} -> liftIO $ throwIO $ userError "undecodable header"
                MissingHeader -> pure $ r { getResponse = acc' }
                Header next -> go (overrideUrl next initial) acc'
    go initial mempty

-- The api

type PaginatedApi = NamedRoutes Foo

data Foo mode = Foo {
    firstContent :: 
        mode 
        :- "contents" 
        :> Get '[JSON] (Headers '[Header "Link" String] [Int]),
    extraContent :: 
        mode 
        :- "contents-extra" 
        :> Capture "page" Int :>  Get '[JSON] (Headers '[Header "Link" String] [Int])
} deriving stock (Generic)


-- The client

fooClient :: Client ClientM PaginatedApi
fooClient = client (Proxy @PaginatedApi)

fooClientDecorated :: Client ClientM PaginatedApi
fooClientDecorated = fooClient { firstContent = paginated fooClient.firstContent}

-- 
fooServer :: Server PaginatedApi
fooServer = Foo {
        firstContent = do
                pure $ addHeader (nextLink 0) [1,2,3]
            , -- :: "contents" :> Get '[JSON] (Headers '[Header "Link" String] [Int]),

        extraContent = \case
            0 -> do
                pure $ addHeader (nextLink 1) [4,5,6]
            1 -> do
                pure $ noHeader [7,8,9]
            _ -> throwError err404
    }
    where 
    nextLink i = uriToString id (fooRoutes.extraContent i) ""

fooRoutes :: MkLink PaginatedApi URI 
fooRoutes = allLinks' (\aLink -> linkURI aLink `relativeTo` baseServerUri)  (Proxy @PaginatedApi)
    where
    baseServerUri = fromJust $ parseAbsoluteURI "http://localhost:8000"

main :: IO ()
main = do
    race_ 
        (do
            putStrLn "Starting server!!!"
            let application :: Application = fooServer & serve (Proxy @PaginatedApi)
            application & run 8000
        )
        (do
            threadDelay 2e6
            manager <- Http.newManager Http.defaultManagerSettings 
            baseUrl <- parseBaseUrl "http://localhost:8000/"
            let clientEnv = mkClientEnv manager baseUrl 
            mr <- runClientM fooClientDecorated.firstContent clientEnv
            case mr of
                Left err -> print err
                Right response -> putStrLn $ "Response: " <> show (getResponse response)
            pure ()
        )