module Lib
	(mainMethod
	)where


import Network.HTTP
import System.Environment           (getArgs, getProgName, lookupEnv)


mainMethod :: String -> IO String
mainMethod url = do
	 simpleHTTP (postRequestWithBody ("http://" ++ url ++ "/storeMessage") ("application/json") ("[{\"name\": \"David\", \"message\": \"hi\" }]")) >>= getResponseBody 
	 simpleHTTP (getRequest "http://" ++ url + "/?=David") >>= getResponseBody 
