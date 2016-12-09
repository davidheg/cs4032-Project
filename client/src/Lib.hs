module Lib
	(mainMethod
	)where



import Data.Aeson
import Data.Proxy
import GHC.Generics
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant
import Servant.API
import Servant.Client
import UseHaskellAPI
import System.Environment(getArgs, getProgName, lookupEnv)


api :: Proxy API
api = Proxy

mainMethod :: String -> IO String
mainMethod url = do
  res <- runClientM queries (ClientEnv manager (BaseUrl Http "10.62.0.213" 8080 ""))
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right (pos, message, em) -> do
      print pos
      print message
      print em

queries :: ClientM (Position, HelloMessage, Email)
queries = do
  pos <- Message "David" "hi" 
  message <- hello (Just "servant") 
  em  <- marketing (ClientInfo "Alp" "alp@foo.com" 26 ["haskell", "mathematics"])
  return (pos, message, em)
  #
data BaseUrl = BaseUrl
  { baseUrlScheme :: Scheme -- ^ URI scheme to use
  , baseUrlHost :: String   -- ^ host (eg "haskell.org")
  , baseUrlPort :: Int      -- ^ port (eg 80)
  }

storeMessage :: Message -> ClientM Bool
searchMessage :: Maybe String -> ClientM [Message]

(loadEnvVars :<|> getREADME :<|> storeMessage :<|> searchMessage :<|> performRestCall) = client api
