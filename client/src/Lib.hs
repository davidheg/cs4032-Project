module Lib
  (mainMethod
  )where

import Network.HTTP
import System.Environment           (getArgs, getProgName, lookupEnv)



mainMethod :: IO String
mainMethod = do 
              getReadme
              storeMessage "David" "Hello"
              searchMessage "David"
              loadEnvironmentVar "David"
              doRestCall Nothing

server = "http://10.62.0.213:8080"
  
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
