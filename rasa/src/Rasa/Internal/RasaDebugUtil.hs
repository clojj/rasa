{-# language OverloadedStrings #-}
module Rasa.Internal.RasaDebugUtil (printStderrThreadsafe) where

import System.IO
import qualified Data.ByteString.Char8 as S8

printStderrThreadsafe :: String -> IO ()
printStderrThreadsafe str = 
  S8.hPutStr stderr (S8.pack str)
