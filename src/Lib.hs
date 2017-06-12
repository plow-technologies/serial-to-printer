{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( printFromSerialPort
    ) where



import qualified Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import System.Hardware.Serialport
import Control.Monad 
import Data.Monoid 
import Data.Foldable
import System.AtomicWrite.Writer.ByteString
import Turtle.Prelude (proc)

port = "/dev/ttyUSB0"  -- Linux
exportFile = "e78prtprt.prt"

printFromSerialPort = do
 s <- openSerial port defaultSerialSettings { timeout = 10 }
 forever $ (buildString s Nothing) >>= (\rslt -> do
                                          _ <- (traverse saveString rslt)
                                          return rslt)
                                   >>= (\rslt -> do
                                          _ <- (traverse printString rslt)
                                          return ())


 closeSerial s

buildString :: SerialPort -> Maybe B.ByteString -> IO (Maybe B.ByteString)
buildString s rslt = do
  val <- recv s 1
  if (val == "") 
     then  return rslt
     else  buildString s  (rslt <> (Just val))

saveString :: B.ByteString -> IO ()
saveString bs = atomicWriteFile exportFile (bs <> "\n\r")

printString :: a -> IO ()
printString _ = do
    proc "lp" ["e78prtprt.prt"] ""
    return ()

