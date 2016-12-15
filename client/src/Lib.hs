{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass     #-}

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
import Data.List
import System.Exit

ip_address = "192.168.60.128"
port_number = 8080
server = "http://192.168.60.128:8080"

data Message = Message { name    :: String
                       , message :: String
                       } deriving (Show, Generic)

instance ToJSON Message where

    toJSON (Message name message) =
        object ["name" .= name, "message" .= message]

mainMethod :: IO()
mainMethod = do
           manager <- newManager tlsManagerSettings
           setGlobalManager manager
           startLoop 

startLoop :: IO()
startLoop = do
           putStrLn "Please enter what task you wish to perform"
           name <- getLine
           selectTask name
           startLoop

selectTask :: String -> IO ()
selectTask "getReadme" = getReadme
selectTask parameters
            |isPrefixOf "searchMessage" parameters = searchMessage (drop 14 parameters)
            |isPrefixOf "storeMessage" parameters = storeMessage (drop 13 parameters)
            |isPrefixOf "loadEnvironmentVar" parameters = loadEnvironmentVar (drop 19 parameters)
            |isPrefixOf "doRestCall" parameters = doRestCall (Just ((drop 11 parameters)))
            |parameters == "exit" = exitWith ExitSuccess

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

storeMessage :: String -> IO()
storeMessage inputs = do
                  print inputs
                  let strings = words inputs
                  let name = strings !! 0
                  let message = strings !! 1
                  initialRequest <-  parseRequest ("POST " ++ server ++ "/storeMessage")
                  let newMessage = Message name message
                  let request= setRequestBodyJSON newMessage initialRequest
                  manager <- getGlobalManager
                  withResponse request manager $ \response  -> do
                    outputResponse response

loadEnvironmentVar :: String -> IO()
loadEnvironmentVar var = do
                  request <- parseRequest (server ++ "/load_environment_variables?name=" ++ var)
                  manager <- getGlobalManager
                  withResponse request manager $ \response  -> do
                    outputResponse response

doRestCall :: Maybe String -> IO()
doRestCall (Just filter) = do
                            request <- parseRequest (server ++ "/performRESTCall?filer=" ++ filter)
                            manager <- getGlobalManager
                            withResponse request manager $ \response  -> do
                              outputResponse response
doRestCall Nothing = do
                      request <- parseRequest (server ++ "/performRESTCall")
                      manager <- getGlobalManager
                      withResponse request manager $ \response  -> do
                        outputResponse response

