-- This file is commented extensively for non-haskell programmers

-- | These are language extensions. Haskell has a great many language
-- extensions but in practice you do not need to knwo much about them. If you
-- use a library that needs them, then the library documentation will tell you which
-- extensions you neeed to include. If you try to write code that needs particular extensions,
-- then the haskell compiler is smart enough typically to be able to suggest which extensions
-- you should switch on by including an entry here.

{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | Haskell code is structured as sets of functions that sit within Modules. The basic rule is that a module with a
-- particular name (for example Lib) sits within a .hs file of the same name (eg. Lib.hs). The module statement is of
-- the form `module MODULE_NAME (EXPORTED_FUNCTIONS) where`. Everything following this is part of the module. There are
-- no brackets or any other syntax to worry about.
module Lib
    ( startApp
    ) where

-- | Imports work like most other languages and are essentially library includes. The functions of the lirbary become
-- immediately accessible in the code of the module. There are various ways in which imports can be modified. For
-- example, one may `import qualified X as Y` which imports a library in such a way that the functions of the library
-- must be prefixed with `Y.`. One can always prefix a libraries functions with the import string, when calling them.
-- You will occasionally have reason to import libraries that have common function names by coincidence. You can use
-- qualified imports of full prefixes to disambiguate. The compiler will tell you where the problem is if this occurs.

import           Control.Concurrent           (forkIO, threadDelay)
import           Control.Monad                (when)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except   (ExceptT)
import           Control.Monad.Trans.Resource
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Bson.Generic
import qualified Data.ByteString.Lazy         as L
import qualified Data.List                    as DL
import           Data.Maybe                   (catMaybes)
import           Data.Text                    (pack, unpack)
import           Data.Time.Clock              (UTCTime, getCurrentTime)
import           Data.Time.Format             (defaultTimeLocale, formatTime)
import           Database.MongoDB
import           GHC.Generics
import           Network.HTTP.Client          (defaultManagerSettings,
                                               newManager)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Logger
import           RestClient
import           Servant
import qualified Servant.API                  as SC
import qualified Servant.Client               as SC
import           System.Environment           (getArgs, getProgName, lookupEnv)
import           System.Log.Formatter
import           System.Log.Handler           (setFormatter)
import           System.Log.Handler.Simple
import           System.Log.Handler.Syslog
import           System.Log.Logger
import           LockAPI

-- | The Servant library has a very elegant model for defining a REST API. We shall demonstrate here. First, we shall
-- define the data types that will be passed in the REST calls. We will define a simple data type that passes some data
-- from client to the server first. There is nothing special about the data being passed - this is a demonstration
-- only. data types are kind of like structs in C or C++.

-- Note that in this version of the project, I have moved the REST API into a shared library called use-haskell-api
-- This library is imported here in order that the HackageAPI type is available to create the REST service of that
-- type. Note that there is no advantage in doing this if you are only building a servant REST service, but if you are
-- creating a corresponding REST client, then following this architectural pattern simplifies development considerably.

-- The relevant code is thus commented out here and the use-haskell-api library content is used instead, and the
-- UseHaskellAPI namespace is imported from that library.

-- data Message = Message { name    :: String
--                        , message :: String
--                        } deriving (Generic, FromJSON, ToBSON, FromBSON)

-- deriving instance FromBSON String  -- we need these as BSON does not provide
-- deriving instance ToBSON   String

-- | We will also define a simple data type for returning data from a REST call, again with nothing special or
-- particular in the response, but instead merely as a demonstration.

-- data ResponseData = ResponseData { response :: String
--                                  } deriving (Generic, ToJSON, FromJSON,FromBSON)

-- | Next we will define the API for the REST service. This is defined as a 'type' using a special syntax from the
-- Servant Library. A REST endpoint is defined by chaining together a series of elements in the format `A :> B :> C`. A
-- set of rest endpoints are chained in the format `X :<|> Y :<|> Z`. We define a set of endpoints to demonstrate
-- functionality as described int he README.md file below.
--
-- Note in the API below that we can mix GET and Post methods. The type of call is determined by the last element in the
-- :> chain. If the method is Get, then the set of QueryParams determine the attributes of the Get call. If the method
-- is Post, then there will be a single ReqBody element that defines the type being transmitted. The return type for
-- each method is noted in the last element in the :> chain.

-- type API = "load_environment_variables" :> QueryParam "name" String :> Get '[JSON] ResponseData
--       :<|> "getREADME"                  :> Get '[JSON] ResponseData
--       :<|> "storeMessage"               :> ReqBody '[JSON] Message  :> Post '[JSON] Bool
--       :<|> "searchMessage"              :> QueryParam "name" String :> Get '[JSON] [ResponseData]
--       :<|> "performRESTCall"            :> QueryParam "filter" String  :> Get '[JSON] ResponseData

-- | Now that we have the Service API defined, we next move on to implementing the service. The startApp function is
-- called to start the application (in fact there is a main function in the file ../app/Main.hs, which is the entry
-- point to the executable, but by convention, it calls startApp in this file. startApp in turn calls a function `app`
-- defined below.
--
-- startApp is defind below on two lines. The first line is a type definition. It states that startApp returns no value
-- (denoted by `()`), but performs Input/Output. Haskell programmes are divided into code that performs I/O and code
-- that does not. The separation is very useful because I/O bound code can return different results even when passed the
-- same parameters (think about it) but non-I/O code will always return the same values for the same parameters. This
-- means we can reason about the correctness of non-I/O code more rigourously, and so the distinction is
-- worthwhile. However, the mixing of I/O and non-I/O code is one of the key problems beginning Haskell programmers have
-- with development of systems. If one fails to annotate type signatures correctly with `IO`, or write IO code
-- incorrectly, the type errors that the compiler generates are quite hard to decypher untill you understand how IO code
-- works. Firtunately its quite simple, and we shall explain below in function definitions.
--
-- A C++, Java or C programmer (amongst others) is used to a formatting of a function or method call of the form `A (X,
-- Y, Z)` where X, Y and Z are parameters passed to the execution of A. Haskell does not use the brackets, such that the
-- app function is defined entirely as a call to the serve function, passing it api and server. Note that these
-- parameters and both functions. In Haskell you can treat functions in an equivalent way to data.
--
-- Note that I have modified the standard implementation of startApp to include logging support, which is explained the
-- the text accompanying the function `withLogging` towards the end of this file.
--
-- I have added a second scheduled thread tot he startApp function to demonstrate how one can create a mutithteaded
-- server. The taskScheduler function is launched before teh startApp function enters its rest loop. The taskScheduler
-- function simply perfroms a loop with a 5 second delay, outputting a warning to the log on each pass.

data File = File { filename :: String 
                 , fileID   :: String
                 } deriving (Generic, ToJSON, FromJSON,FromBSON, Read, Eq, Show)


startApp :: IO ()    -- set up wai logger for service to output apache style logging for rest calls
startApp = withLogging $ \ aplogger -> do
  warnLog "Starting use-haskell."

  forkIO $ taskScheduler 5

  let settings = setPort 8080 $ setLogger aplogger defaultSettings
  runSettings settings app

-- this is the original startApp that stack new servant builds
--startApp :: IO ()
--startApp = run 8080 app

taskScheduler :: Int -> IO ()
taskScheduler delay = do
  warnLog $ "Task scheduler operating."

  threadDelay $ delay * 1000000
  taskScheduler delay -- tail recursion

-- | the function app calls serve, passing api and server. You can see that there is a kind of structure of function
-- composition here that is to do with how servant operates. The first parameter defines the type of the REST service,
-- (which is defined as type `Proxy API` - yes types of this formt are allowed - and we should note that the second part
-- of that type is out previously defined REST API) and the second parameter defines the implementation of the REST service.
app :: Application
app = serve api server

api :: Proxy API
api = Proxy

-- | And now we implement the REST service by matching the API type, providing a Handler method for each endpoint
-- defined in the API type. Normally we would implement each endpoint using a unique method definition, but this need
-- not be so. To add a news endpoint, define it in type API above, and add and implement a handler here.
server :: Server API
server = requestLock
    :<|> returnLock

  where
    -- | where is just a way of ensuring that the following functions are scoped to the server function. Each function
    -- below hasa type matching the end point parameters, with the return type being of type `Handler a`, where a is the
    -- type of data returned by the endpoint. Handler is a Monad. This may strike terror - it should not, but th great
    -- news is that we don't have to understand Monads at all really, providing we follow some simple programming
    -- conventions when writing handlers.
    --
    -- Now we will explain each handler in turn, along with any helper functions we write to help us implement the
    -- various endpoints.
   
    requestLock :: LockRequest -> Handler (Maybe Lock)
    requestLock request@(LockRequest filename username) = liftIO $ do
      warnLog $ "User " ++ username ++ " requesting access to file " ++ filename
      files <- withMongoDbConnection $ do
          docs <- find (select ["filename" =: filename] "FILE_RECORD") >>= drainCursor
          return $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe File) docs
      let file = files !! 0 
      let fileID = getFileID file
      lock <- withMongoDbConnection $ do
            docs <- find (select ["fileID" =: fileID] "LOCK_RECORD") >>= drainCursor
             --warnLog $ "retrieved data: " ++ show docs
            return $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe Lock) docs
      let currentLock = lock !! 0
      let user = getLockUser currentLock
      if user == ""
        then do
          let newLock = (Lock fileID username)
          withMongoDbConnection $ upsert (select ["fileID" =: fileID] "LOCK_RECORD") $ toBSON newLock
          return (Just newLock)
        else
          return Nothing

      -- upsert creates a new record if the identified record does not exist, or if
      -- it does exist, it updates the record with the passed document content
      -- As you can see, this takes a single line of code

    returnLock :: Lock ->  Handler Bool
    returnLock lock@(Lock fileID user) = liftIO $ do
        locks <- withMongoDbConnection $ do
          docs <- find (select ["fileID" =: fileID] "LOCK_RECORD") >>= drainCursor
          return $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe Lock) docs
        let currentLock = locks !! 0
        let currentUser = getLockUser currentLock
        if currentUser == user
          then do 
            let newLock = Lock fileID ""
            withMongoDbConnection $ upsert (select ["fileID" =: fileID] "LOCK_RECORD") $ toBSON newLock
            return True
          else do
            return False

    -- | Performing a REST call
    -- The following function performs a REST call to a remote service 'hackage.haskell.org'. This remote service is a
    -- searchable documentation server. The API to the service is accessible at http://hackage.haskell.org
  

-- What follows next is some helper function code that makes it easier to do warious things such as use
-- a mongoDB, post console log statements define environment variables for use in your programmes and so forth.
-- The code is not written particularly to be understood by novice Haskellers, but should be useable relatively easily
-- as set out above.

-- | error stuff

getFileID :: File -> String
getFileID (File _ fileID) = return fileID !! 0

getLockUser :: Lock -> String
getLockUser (Lock _ user) = return user !! 0

custom404Error request = err404 { errBody = request }


-- | Logging stuff
iso8601 :: UTCTime -> String
iso8601 = formatTime defaultTimeLocale "%FT%T%q%z"

-- global loggin functions
debugLog, warnLog, errorLog :: String -> IO ()
debugLog = doLog debugM
warnLog  = doLog warningM
errorLog = doLog errorM
noticeLog = doLog noticeM

doLog f s = getProgName >>= \ p -> do
                t <- getCurrentTime
                f p $ (iso8601 t) ++ " " ++ s

withLogging act = withStdoutLogger $ \aplogger -> do

  lname  <- getProgName
  llevel <- logLevel
  updateGlobalLogger lname
                     (setLevel $ case llevel of
                                  "WARNING" -> WARNING
                                  "ERROR"   -> ERROR
                                  _         -> DEBUG)
  act aplogger


-- | Mongodb helpers...

-- | helper to open connection to mongo database and run action
-- generally run as follows:
--        withMongoDbConnection $ do ...
--
withMongoDbConnection :: Action IO a -> IO a
withMongoDbConnection act  = do
  ip <- mongoDbIp
  port <- mongoDbPort
  database <- mongoDbDatabase
  pipe <- connect (host ip)
  ret <- runResourceT $ liftIO $ access pipe master (pack database) act
  close pipe
  return ret

-- | helper method to ensure we force extraction of all results
-- note how it is defined recursively - meaning that draincursor' calls itself.
-- the purpose is to iterate through all documents returned if the connection is
-- returning the documents in batch mode, meaning in batches of retruned results with more
-- to come on each call. The function recurses until there are no results left, building an
-- array of returned [Document]
drainCursor :: Cursor -> Action IO [Document]
drainCursor cur = drainCursor' cur []
  where
    drainCursor' cur res  = do
      batch <- nextBatch cur
      if null batch
        then return res
        else drainCursor' cur (res ++ batch)

-- | Environment variable functions, that return the environment variable if set, or
-- default values if not set.

-- | The IP address of the mongoDB database that devnostics-rest uses to store and access data
mongoDbIp :: IO String
mongoDbIp = defEnv "MONGODB_IP" id "database" True

-- | The port number of the mongoDB database that devnostics-rest uses to store and access data
mongoDbPort :: IO Integer
mongoDbPort = defEnv "MONGODB_PORT" read 27017 False -- 27017 is the default mongodb port

-- | The name of the mongoDB database that devnostics-rest uses to store and access data
mongoDbDatabase :: IO String
mongoDbDatabase = defEnv "MONGODB_DATABASE" id "USEHASKELLDB" True

-- | Determines log reporting level. Set to "DEBUG", "WARNING" or "ERROR" as preferred. Loggin is
-- provided by the hslogger library.
logLevel :: IO String
logLevel = defEnv "LOG_LEVEL" id "DEBUG" True


-- | Helper function to simplify the setting of environment variables
-- function that looks up environment variable and returns the result of running funtion fn over it
-- or if the environment variable does not exist, returns the value def. The function will optionally log a
-- warning based on Boolean tag
defEnv :: Show a
              => String        -- Environment Variable name
              -> (String -> a)  -- function to process variable string (set as 'id' if not needed)
              -> a             -- default value to use if environment variable is not set
              -> Bool          -- True if we should warn if environment variable is not set
              -> IO a
defEnv env fn def doWarn = lookupEnv env >>= \ e -> case e of
      Just s  -> return $ fn s
      Nothing -> do
        when doWarn (doLog warningM $ "Environment variable: " ++ env ++
                                      " is not set. Defaulting to " ++ (show def))
        return def




