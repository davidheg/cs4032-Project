{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  (mainMethod
  )where

import qualified Data.ByteString           as S
import Network.HTTP
import Network.HTTP.Simple
import Network.HTTP.Client
import Network.HTTP.Client.TLS 
import Network.HTTP.Types.Status (statusCode)
import System.Environment      (getArgs, getProgName, lookupEnv)
import System.IO                 (stdout)
import Data.Aeson   
import GHC.Generics

ip_address = "192.168.60.128"
port_number = 8080
server = "http://192.168.60.128:8080"

data Message = Message {
      name :: String
    , message  :: String
} deriving (Generic, Show)

instance ToJSON Message where

    toJSON (Message name age) =
        object ["name" .= name, "age" .= age]

mainMethod :: IO()
mainMethod = do
           --request <- parseRequest (server ++ "/searchMessage?name=David")
           --withResponse request manager $ \response -> do
           manager <- newManager tlsManagerSettings
           setGlobalManager manager
           --putStrLn "Please enter what task you wish to perform"
           --name <- getLine
           --selectTask name
           storeMessage "David" "Hi"
           --mainMethod

selectTask :: String -> IO ()
selectTask "getReadme" = getReadme
--selectTask "REST" = doRestCall Nothing
--selectTask _ = select

outputResponse ::Network.HTTP.Client.Response BodyReader -> IO ()
outputResponse response = do 
                  let loop = do
                        bs <- brRead $ responseBody response
                        if S.null bs
                            then putStrLn "\nFinished response body"
                            else do
                                S.hPut stdout bs
                                loop
                  loop
getReadme ::IO ()
getReadme = do 
        request <- parseRequest (server ++ "/getREADME")
        manager <- getGlobalManager
        withResponse request manager $ \response  -> do
          outputResponse response

searchMessage :: String -> IO()
searchMessage name = do
                request <- parseRequest (server ++ "/searchMessage?name=" ++ name)
                manager <- getGlobalManager
                withResponse request manager $ \response  -> do
                  outputResponse response

storeMessage :: String -> String -> IO()
storeMessage name message = do
                  initialRequest <-  parseRequest ("POST " ++ server ++ "/storeMessage")
                  let request= setRequestBodyJSON message initialRequest
                  manager <- getGlobalManager
                  withResponse request manager $ \response  -> do
                    outputResponse response

--loadEnvironmentVar :: String -> IO String
--loadEnvironmentVar name = do
  --                simpleHTTP (getRequest (server ++ "/load_environment_variables?name=" ++ name)) >>= getResponseBody

--doRestCall :: Maybe String -> IO String
--doRestCall (Just filter) = do
--                       simpleHTTP (getRequest (server ++ "/performRESTCall?filter=" ++ filter)) >>=getResponseBody
--doRestCall Nothing = do
--            simpleHTTP (getRequest (server  ++ "/performRESTCall")) >>=getResponseBody


