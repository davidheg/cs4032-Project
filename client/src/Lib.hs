{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Lib
  (mainMethod
  )where

import qualified Data.ByteString           as S
import Data.ByteString.Char8     (pack, unpack)
import qualified Data.ByteString.Lazy.Char8 as L
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
import Data.Maybe
import Data.Strings
import System.Exit
import UseHaskellAPI
import AuthenticationAPI
import Data.String.Utils
import Control.Concurrent
import Data.Time.Clock 
import Data.Time.Format
import Crypto.Cipher.AES

ip_address = "192.168.60.128"
port_number = 8080
file_server = "http://192.168.60.128:8080"
authentication_server = "http://192.168.60.128:2020"
currentUser = AuthenticationAPI.UserInfo "" ""
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
            |isPrefixOf "uploadFile" parameters = uploadFile (drop 10 parameters)
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
          let inputs = words (strip input)
          let key =  initAES (pack (padString (inputs !! 1)))
          let currentUser = AuthenticationAPI.UserInfo (inputs !! 0) (inputs !! 1)
          let password = unpack (encryptECB key (pack (padString (inputs !! 1))))
          let loginReq = LoginRequest (show (AuthenticationAPI.UserInfo (inputs !! 0) password))
          initialRequest <-  parseRequest ("GET " ++ authentication_server ++ "/login")
          let request = setRequestBodyJSON loginReq initialRequest
          manager <- getGlobalManager
          response <- Network.HTTP.Client.httpLbs request manager   
          let loginMonad = (decode ((responseBody response))) :: Maybe LoginResponse 
          case loginMonad of
            Just loginResponse -> do
              let encryptedToken = getToken loginResponse
              let decryptedtoken = unpack (decryptECB key (pack (padString encryptedToken)))
              print decryptedtoken
              let token@(Token ticket key time user) = read (strip decryptedtoken) :: Token
              let userInfo = read (strip user) :: AuthenticationAPI.UserInfo
              print ("This is the key on receival: " ++ key)
              let sessionKey = initAES (pack key)
              print ("This is the ticket on receival: " ++ ticket)
              writeFile "UserDetails" ((getUsername userInfo) ++ "|" ++ key ++ "|" ++ (strip ticket))
              print "Login Successful"
            Nothing -> do 
              putStrLn "Login Unsuccessful, please try entering your username and password again"
              inputs <- getLine
              login inputs

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

searchFiles :: String -> IO()
searchFiles filename = do
                initialRequest <- parseRequest ("POST " ++ file_server ++ "/searchFiles")
                details <- readFile "UserDetails"
                let dets = read ("\"" ++ details ++ "\"") :: String
                let userDetails = split "|" dets
                let user = userDetails !! 0
                let key = userDetails !! 1
                let ticket = userDetails !! 2
                let sessionKey = initAES (pack key)
                let encryptedFile = unpack (encryptECB sessionKey (pack (padString (show filename))))  
                let search = (EncryptedMessage user filename ticket)
                let request = setRequestBodyJSON search initialRequest
                manager <- getGlobalManager
                withResponse request manager $ \response  -> do
                  outputResponse response

uploadFile :: String -> IO()
uploadFile inputs = do
                  let path = strip inputs
                  let components = split "/" path
                  let i = length components
                  let name = components !! (i -1)
                  details <- readFile "UserDetails"
                  let userDetails = split "|" details
                  print userDetails
                  print (length userDetails)
                  let user = strip (userDetails !! 0)
                  let key = (strip (userDetails !! 1))
                  let ticket = userDetails !! 2
                  print ("This is the ticket when its loaded from the file: " ++ ticket)
                  putStrLn "Please enter the usernames of anyone you would like to share to the file with"
                  names <- getLine
                  let users = user ++ " " ++ names 
                  initialRequest <-  parseRequest ("POST " ++ file_server ++ "/uploadFile")
                  contents <- readFile path
                  let newFile = (UserFile name path users contents)
                  print ("This is the key after being loaded from the file: " ++ key)
                  let sessionKey = initAES (pack key) 
                  let encryptedFile = unpack (encryptECB sessionKey (pack (padString (show newFile))))
                  let userRequest = (EncryptedMessage user encryptedFile ticket)
                  let request = setRequestBodyJSON userRequest initialRequest
                  currentTime <- getCurrentTime
                  let newUploaded = (FileTime name (getTime currentTime))
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

getUsername :: AuthenticationAPI.UserInfo -> String
getUsername (AuthenticationAPI.UserInfo name _) = return name !! 0

getName :: FileTime -> String
getName _ = ""
getName (FileTime name _) = name

addFile :: [FileTime] -> FileTime -> [FileTime]
addFile [] file = [file]
addFile files new = (files ++ [new])

getTime :: UTCTime -> String
getTime = formatTime defaultTimeLocale "%FT%T%q%z"

padString :: String -> String
padString input
      |mod (strLen (input)) 16 == 0 = input
      |otherwise = padString (input ++ " ")

getToken :: LoginResponse -> String
getToken (LoginResponse token) = return token !! 0
