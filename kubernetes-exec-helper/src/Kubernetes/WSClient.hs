{-# LANGUAGE OverloadedStrings #-}

module WSClient
    (
      -- * Client connection 
      kClient
      -- * App
      , runClient
      , readLine
      , readResize
      , readStdIn
      , readStdOut 
      , readStdErr
      , readErr
      , isOpen
    )
  where 

import Control.Exception
import Control.Concurrent(forkIO)
import Control.Concurrent.STM
import Control.Monad (forever, unless)
import Data.Text (Text)
import Network.Socket(withSocketsDo)
import qualified Data.Text as T
import qualified Network.WebSockets as WS
import System.Timeout


type Channels = (
    TChan Text -- * STDIN
    , TChan Text -- * STDOut
    , TChan Text -- * STDError
    , TChan Text -- * Error
    , TChan Text -- * Resize
    )

isOpen :: WS.Connection -> IO Bool 
isOpen = undefined



_timeout :: TimeoutInterval 
_timeout = 30 * 1000000
runClient :: String -> Int -> String -> IO ([Text], [Text], [Text], [Text], [Text])
runClient domain port route = 
  withSocketsDo $ WS.runClient domain port route (\c -> kClient c _timeout)

type TimeoutInterval = Int
{-- 
  . Read from a socket. 
  . Parse the message and write on to an appropriate channel
  . Start reader threads to read from the channel.
  . Deal with asynchronous exceptions during disconnects.
--}

kClient :: WS.Connection -> TimeoutInterval -> IO ([Text], [Text], [Text], [Text], [Text])
kClient conn timeoutInterval = do
    stdIn <- atomically newTChan
    stdOut <- atomically newTChan 
    stdErr <- atomically newTChan 
    err <- atomically newTChan 
    resize <- atomically newTChan
    rcv <- timeout timeoutInterval $ forkIO $ forever $ do
        msg <- (WS.receiveData conn) `catch` (\e@(SomeException s) -> return "")
        writeMsg (stdIn, stdOut, stdErr, err, resize) $ T.splitAt 1 msg
    WS.sendClose conn ("Bye!" :: T.Text)
    drainChannels (stdIn, stdOut, stdErr, err, resize)

drainChannels :: Channels -> IO ([Text], [Text], [Text], [Text], [Text])
drainChannels (stdIn, stdOut, stdErr, err, resize) = do 
  inBuf <- channelReader stdIn
  outBuf <- channelReader stdOut
  stdErrBuf <- channelReader stdErr
  errBuf <- channelReader err 
  resizeBuf <- channelReader resize
  return (inBuf, outBuf, stdErrBuf, errBuf, resizeBuf)

channelReader :: TChan Text -> IO [Text]
channelReader inChan = do 
  loop inChan []
  where 
    loop inChan l@(h : t) = do 
      nextItem <- atomically . readTChan $ inChan
      emptyTChan <- atomically . isEmptyTChan $ inChan
      if (emptyTChan) then
        return l
      else
        loop inChan (nextItem : l)

writeMsg :: (TChan Text, TChan Text, TChan Text, TChan Text, TChan Text) -> (Text, Text) -> IO ()
writeMsg (stdIn, stdOut, stdErr, err, resize) (channel, message) = 
  case channel of
    "0" -> atomically $ writeTChan stdIn message 
    "1" -> atomically $ writeTChan stdOut message 
    "2" -> atomically $ writeTChan stdErr message 
    "3" -> atomically $ writeTChan err message 
    "4" -> atomically $ writeTChan resize message 
    _ -> return ()

readStdIn :: Channels -> STM Text
readStdIn (stdIn, _, _, _, _) = readTChan stdIn

readStdOutSTM :: Channels -> STM Text
readStdOutSTM (_, stdOut, _, _, _) = readTChan stdOut 

readStdOut :: Channels -> IO Text 
readStdOut = atomically . readStdOutSTM

readStdErrSTM :: Channels -> STM Text
readStdErrSTM (_, _, stdErr, _, _) = readTChan stdErr 

readStdErr :: Channels -> IO Text 
readStdErr = atomically . readStdErrSTM

readErrSTM :: Channels -> STM Text 
readErrSTM (_, _, _, err, _) = readTChan err 

readErr :: Channels -> IO Text 
readErr = atomically . readErrSTM

readResizeSTM :: Channels -> STM Text 
readResizeSTM (_, _, _, _, resize) = readTChan resize

readResize :: Channels -> IO Text 
readResize = atomically . readResizeSTM

readLineSTM :: TChan Text -> STM Text
readLineSTM aChannel = do 
  messages <- (T.split (\c -> c == '\n')) <$> readTChan aChannel 
  case messages of 
    h : t -> do 
        unGetTChan aChannel $ T.unlines t
        return h 
    _ -> return ""

readLine :: TChan Text -> IO Text 
readLine = atomically . readLineSTM