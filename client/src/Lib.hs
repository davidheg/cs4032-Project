{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

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
import Data.List.Utils
import System.Exit
import UseHaskellAPI
import AuthenticationAPI
import Data.String.Utils
import Control.Concurrent
import Data.Time.Clock 
import Data.Time.Format 


ip_address = "192.168.60.128"
port_number = 8080
file_server = "http://192.168.60.128:8080"
authentication_server = "http://192.168.60.128:2020"
currentUser = AuthenticationAPI.UserInfo "davidheg" "distro"
files = []

mainMethod :: IO()
mainMethod = do
          putStrLn "Would you like to login or register?"
          action <- getLine
          if (strip action) == "register" 
            then do
              putStrLn "Please enter a username and password"
              inputs <- getLine
              register inputs
              login inputs
            else do
              putStrLn "Please enter your username and password"
              details <- getLine
              login details
          manager <- newManager tlsManagerSettings
          setGlobalManager manager
          let check = forkIO pollServer
          startLoop

startLoop :: IO ()
startLoop = do
           putStrLn "Please enter what task you wish to perform"
           name <- getLine
           selectTask name
           startLoop

pollServer :: IO ()
pollServer = do
                polling files
                pollServer

polling :: [FileTime] -> IO ()
polling [] = return ()
polling (x:xs) = do 
                request <- parseRequest (file_server ++ "/updatedFiles?filename=" ++ getName x)
                manager <- getGlobalManager
                withResponse request manager $ \response  -> do
                  outputResponse response 


selectTask :: String -> IO ()
selectTask parameters
            |isPrefixOf "uploadFile" parameters = uploadFileType2 (drop 10 parameters)
            |isPrefixOf "searchFiles" parameters = searchFiles (drop 11 parameters)
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

register :: String -> IO ()
register input = do 
          let inputs = words (strip input)
          let user = AuthenticationAPI.UserInfo (inputs !! 0) (inputs !! 1)
          initialRequest <-  parseRequest ("POST " ++ authentication_server ++ "/register")
          let request = setRequestBodyJSON user initialRequest
          manager <- getGlobalManager
          withResponse request manager $ \response  -> do
            outputResponse response

login :: String -> IO ()
login input = do
          let loginReq = LoginRequest input "key"
          initialRequest <-  parseRequest ("GET " ++ authentication_server ++ "/login")
          let request = setRequestBodyJSON loginReq initialRequest
          manager <- getGlobalManager
          withResponse request manager $ \response  -> do
            outputResponse response 
getReadme ::IO ()
getReadme = do 
        request <- parseRequest (file_server ++ "/getREADME")
        manager <- getGlobalManager
        withResponse request manager $ \response  -> do
          outputResponse response

searchMessage :: String -> IO()
searchMessage name = do
                request <- parseRequest (file_server ++ "/searchMessage?name=" ++ name)
                manager <- getGlobalManager
                withResponse request manager $ \response  -> do
                  outputResponse response

storeMessage :: String -> IO()
storeMessage inputs = do
                  let strings = words inputs
                  let name = strings !! 0
                  let message = strings !! 1
                  initialRequest <-  parseRequest ("POST " ++ file_server ++ "/storeMessage")
                  let newMessage = Message name message
                  let request= setRequestBodyJSON newMessage initialRequest
                  manager <- getGlobalManager
                  withResponse request manager $ \response  -> do
                    outputResponse response

uploadFile :: String -> IO()
uploadFile inputs = do
                  let strings = words (strip inputs)
                  let user = strings !! 0
                  let path = strings !! 1
                  let components = split "/" path
                  let i = length components
                  let name = components !! (i -1)
                  putStrLn "Please enter the usernames of anyone you would like to share to the file with"
                  names <- getLine
                  let users =  user ++ " " ++ names 
                  initialRequest <-  parseRequest ("POST " ++ file_server ++ "/uploadFile")
                  contents <- readFile path
                  let content = read ("\"" ++ contents ++ "\"") :: String
                  let newFile = UserFile name path users content
                  let request= setRequestBodyJSON newFile initialRequest
                  manager <- getGlobalManager
                  withResponse request manager $ \response  -> do
                    outputResponse response

searchFiles :: String -> IO()
searchFiles filename = do
                request <- parseRequest (file_server ++ "/searchFiles?filename=" ++ filename)
                manager <- getGlobalManager
                withResponse request manager $ \response  -> do
                  outputResponse response

uploadFileType2 :: String -> IO()
uploadFileType2 inputs = do
                  let strings = words (strip inputs)
                  let path = strings !! 0
                  let components = split "/" path
                  let i = length components
                  let name = components !! (i -1)
                  putStrLn "Please enter the usernames of anyone you would like to share to the file with"
                  names <- getLine
                  let users =  "davidheg" ++ " " ++ names 
                  initialRequest <-  parseRequest ("POST " ++ file_server ++ "/fileTypeTwo")
                  contents <- readFile path
                  let content = read ("\"" ++ contents ++ "\"") :: String
                  let newFile = name ++ "|" ++ path ++ "|" ++ users ++ "|" ++ content
                  let user = "davidheg|distro"
                  let userRequest = UserRequest user newFile
                  let request= setRequestBodyJSON userRequest initialRequest
                  currentTime <- getCurrentTime
                  let newUploaded = FileTime name (getTime currentTime)
                  let files = addFile files newUploaded
                  manager <- getGlobalManager
                  withResponse request manager $ \response  -> do
                    outputResponse response

loadEnvironmentVar :: String -> IO()
loadEnvironmentVar var = do
                  request <- parseRequest (file_server ++ "/load_environment_variables?name=" ++ var)
                  manager <- getGlobalManager
                  withResponse request manager $ \response  -> do
                    outputResponse response

doRestCall :: Maybe String -> IO()
doRestCall (Just filter) = do
                            request <- parseRequest (file_server ++ "/performRESTCall?filer=" ++ filter)
                            manager <- getGlobalManager
                            withResponse request manager $ \response  -> do
                              outputResponse response
doRestCall Nothing = do
                      request <- parseRequest (file_server ++ "/performRESTCall")
                      manager <- getGlobalManager
                      withResponse request manager $ \response  -> do
                        outputResponse response

getUsernames :: String -> String -> [String]
getUsernames user [] = return user
getUsernames user names = merge [user] (words (strip names))

getName :: FileTime -> String
getName _ = ""
getName (FileTime name _) = name

addFile :: [FileTime] -> FileTime -> [FileTime]
addFile [] file = [file]
addFile files new = (files ++ [new])

getTime :: UTCTime -> String
getTime = formatTime defaultTimeLocale "%FT%T%q%z"