module Main where

import Network.HTTP.Client.Internal qualified as Http
import Servant.Client
import Servant.API
import Servant.API.NamedRoutes
import Servant.Server
import Servant.Links
import Data.Proxy
import GHC.Generics (Generic)

type PaginatedApi = NamedRoutes Foo

data Foo mode = Foo {
    firstContent :: mode :- "contents" :> Get '[JSON] (Headers '[Header "Link" String] [Int]),
    extraContent :: mode :- "contents-extra" :> QueryParam "page" Int :>  Get '[JSON] (Headers '[Header "Link" String] [Int])
} deriving stock (Generic)

fooServer :: Server PaginatedApi
fooServer = Foo {
        firstContent = undefined, -- :: "contents" :> Get '[JSON] (Headers '[Header "Link" String] [Int]),
        extraContent = \i -> undefined
    }

fooRoutes :: MkLink PaginatedApi URI 
fooRoutes = allLinks' linkURI (Proxy @PaginatedApi)

main :: IO ()
main = putStrLn "Hello, Haskell!"
