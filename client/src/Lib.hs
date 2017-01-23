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
import Data.String.Utils
import Control.Concurrent
import Data.Time.Clock 


ip_address = "192.168.60.128"
port_number = 8080
server = "http://192.168.60.128:8080"
currentUser = UserInfo "davidheg" "distro"
files = []

data FileUploaded = FileUploaded { filename :: String
                                 , time :: IO UTCTime
                                 } 

mainMethod :: IO()
mainMethod = do
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

polling :: [FileUploaded] -> IO ()
polling [] = return ()
polling (x:xs) = do 
                request <- parseRequest (server ++ "/updatedFiles?filename=" ++ getName x)
                manager <- getGlobalManager
                withResponse request manager $ \response  -> do
                  outputResponse response 


selectTask :: String -> IO ()
selectTask "getReadme" = getReadme
selectTask parameters
            |isPrefixOf "searchMessage" parameters = searchMessage (drop 14 parameters)
            |isPrefixOf "storeMessage" parameters = storeMessage (drop 13 parameters)
            |isPrefixOf "loadEnvironmentVar" parameters = loadEnvironmentVar (drop 19 parameters)
            |isPrefixOf "doRestCall" parameters = doRestCall (Just ((drop 11 parameters)))
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
                  brRead $ responseBody response

storeMessage :: String -> IO()
storeMessage inputs = do
                  let strings = words inputs
                  let name = strings !! 0
                  let message = strings !! 1
                  initialRequest <-  parseRequest ("POST " ++ server ++ "/storeMessage")
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
                  initialRequest <-  parseRequest ("POST " ++ server ++ "/uploadFile")
                  contents <- readFile path
                  let content = read ("\"" ++ contents ++ "\"") :: String
                  let newFile = UserFile name path users content
                  let request= setRequestBodyJSON newFile initialRequest
                  manager <- getGlobalManager
                  withResponse request manager $ \response  -> do
                    outputResponse response

searchFiles :: String -> IO()
searchFiles filename = do
                request <- parseRequest (server ++ "/searchFiles?filename=" ++ filename)
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
                  initialRequest <-  parseRequest ("POST " ++ server ++ "/fileTypeTwo")
                  contents <- readFile path
                  let content = read ("\"" ++ contents ++ "\"") :: String
                  let newFile = name ++ "|" ++ path ++ "|" ++ users ++ "|" ++ content
                  let user = "davidheg|distro"
                  let userRequest = UserRequest user newFile
                  let request= setRequestBodyJSON userRequest initialRequest
                  let newUploaded = FileUploaded name getCurrentTime
                  let files = addFile files newUploaded
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

getUsernames :: String -> String -> [String]
getUsernames user [] = return user
getUsernames user names = merge [user] (words (strip names))

getName :: FileUploaded -> String
getName _ = ""
getName (FileUploaded name _) = name

addFile :: [FileUploaded] -> FileUploaded -> [FileUploaded]
addFile [] file = [file]
addFile files new = (files ++ [new])