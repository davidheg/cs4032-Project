module Lib
  (mainMethod
  )where

import qualified Data.ByteString           as S
import Network.HTTP
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import System.Environment      (getArgs, getProgName, lookupEnv)
import           System.IO                 (stdout)

ip_address = "10.62.0.213"
port_number = 8080
server = "http://10.62.0.213:8080"

--data tlsManagerSettings = Settings
--    { settingsPort :: Int
--    , settingsHost :: String
--    , settingsTimeout :: Int
--    }

defaultSettings = ManagerSettings 8080 "10.62.0.213" 30
manager = newManager defaultSettings

mainMethod :: IO String
mainMethod = do
           request <- parseRequest (server ++ "/getREADME")
           withResponse request manager $ \response -> do
              putStrLn $ "The status code was: " ++
                   show (statusCode $ responseStatus response)

              let loop = do
                      bs <- brRead $ responseBody response
                      if S.null bs
                          then putStrLn "\nFinished response body"
                          else do
                              S.hPut stdout bs
                              loop
              loop
           putStrLn "Please enter what task you wish to perform"
           name <- getLine
           selectTask name

selectTask :: String -> IO String
selectTask "getReadme" = getReadme
selectTask "REST" = doRestCall Nothing
--selectTask _ = select

getReadme :: IO String
getReadme = do simpleHTTP (getRequest (server ++ "/getREADME")) >>= getResponseBody

searchMessage :: String -> IO String
searchMessage name = do
            simpleHTTP  (getRequest (server ++ "/searchMessage?name=" ++ name)) >>=getResponseBody

storeMessage :: String -> String -> IO String
storeMessage name message = do
                  simpleHTTP (postRequestWithBody (server ++ "/storeMessage")("application/json") ("{\"name\":\"" ++ name ++ "\",\"message\": \"" ++ message ++ "\"}")) >>= getResponseBody


loadEnvironmentVar :: String -> IO String
loadEnvironmentVar name = do
                  simpleHTTP (getRequest (server ++ "/load_environment_variables?name=" ++ name)) >>= getResponseBody

doRestCall :: Maybe String -> IO String
doRestCall (Just filter) = do
                       simpleHTTP (getRequest (server ++ "/performRESTCall?filter=" ++ filter)) >>=getResponseBody
doRestCall Nothing = do
            simpleHTTP (getRequest (server  ++ "/performRESTCall")) >>=getResponseBody


