{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Data.Aeson            (decode, encode, parseJSON, toJSON)
import           Data.Maybe            (fromJust)
import           Data.Yaml             (decodeFile)
import           Kubernetes.KubeConfig (AuthInfo (..), Cluster (..), Config,
                                        Context (..), getAuthInfo, getCluster,
                                        getContext)
import           Test.Hspec



-- Create a test server, a test client and send some messages.
-- Close. 
-- This should all work.
testSetup :: IO () = do
  -- create test server.
  -- create test client.
  -- Send some messages.


main :: IO ()
main = do 
  putStrLn "test.."