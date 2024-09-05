{-# LANGUAGE OverloadedRecordDot #-}
module Main where

import Network.HTTP.Client.Internal qualified as Http
import Servant.Client
import Servant.API
import Servant.API.NamedRoutes
import Servant.Server
import Servant.Links
import Data.Proxy
import Data.Text
import GHC.Generics (Generic)
import Network.URI (uriToString)
import Data.Function ((&))
import Network.Wai.Handler.Warp (run)
import Servant (throwError)

type PaginatedApi = NamedRoutes Foo

data Foo mode = Foo {
    firstContent :: mode :- "contents" :> Get '[JSON] (Headers '[Header "Link" String] [Int]),
    extraContent :: mode :- "contents-extra" :> Capture "page" Int :>  Get '[JSON] (Headers '[Header "Link" String] [Int])
} deriving stock (Generic)

fooServer :: Server PaginatedApi
fooServer = Foo {
        firstContent = do
                pure $ addHeader (nextLink 0) [1,2,3]
            , -- :: "contents" :> Get '[JSON] (Headers '[Header "Link" String] [Int]),

        extraContent = \case
            0 -> do
                pure $ addHeader (nextLink 1) [3,4,5]
            1 -> do
                pure $ noHeader [3,4,5]
            _ -> throwError err404
    }
    where 
    nextLink i = uriToString id (fooRoutes.extraContent i) ""

fooRoutes :: MkLink PaginatedApi URI 
fooRoutes = allLinks' linkURI (Proxy @PaginatedApi)

main :: IO ()
main = do
    putStrLn "Starting server!!!"
    let application :: Application = fooServer & serve (Proxy @PaginatedApi)
    application & run 8000