module Main where

import Rasa (rasa)
import Control.Lens
import Rasa.Ext
import Rasa.Ext.Cursors
import Rasa.Ext.Files
import Rasa.Ext.Logger
import Rasa.Ext.Slate
import Rasa.Ext.StatusBar
import Rasa.Ext.Style
import Rasa.Ext.Vim

import Control.Monad.IO.Class

import ErrUtils (mkPlainErrMsg)
import FastString (mkFastString)
import GHC
import GHC.Paths (libdir)
import Lexer
import qualified MonadUtils as GMU
import SrcLoc
import StringBuffer

import Control.Concurrent
import Control.Concurrent.Chan ()
import Control.Monad (forever)

import Data.Default
import qualified Data.Text as T
import System.IO

-- | This is the main of an executable that runs rasa with any extensions the
-- user wants
--
-- The @do@ block is of type 'Rasa.Ext.Scheduler.Scheduler'
main :: IO ()
main =
  rasa $
  do onInit initLexer
     vim
     statusBar
     files
     cursors
     logger
     slate
     beforeRender lexHaskell
     style


lexHaskell :: Action ()
lexHaskell = 
  focusDo $ do
    LexChan ch <- use bufExt
    src <- use text
    liftIO $ writeChan ch (T.unpack src)
    return ()


newtype LexChan = LexChan (Chan String)
  -- deriving Show

instance Show LexChan where
  show (LexChan _) = "Lexer Channel"

instance Default LexChan where
  def = LexChan undefined
  
initLexer :: Action ()
initLexer = focusDo startGhc

startGhc :: BufAction ()
startGhc = do
  ch <- liftIO $ newChan
  bufExt .= LexChan ch
  liftIO $ do
    tid <- forkIO $ runGhc (Just libdir) $
      do flags <- getSessionDynFlags
         let lexLoc = mkRealSrcLoc (mkFastString "<interactive>") 1 1
         GMU.liftIO $
           forever $
           do str <- readChan ch
              let sb = stringToStringBuffer str
              let pResult = lexTokenStream sb lexLoc flags
              case pResult
                   -- POk _ toks    -> GMU.liftIO $ putStr $ concatMap showToken toks
                    of
                POk _ toks ->
                  GMU.liftIO $
                  hPutStr stderr $ concatMap showTokenWithSource (addSourceToTokens lexLoc sb toks)
                PFailed srcspan msg -> do
                  GMU.liftIO $ print $ show srcspan
                  GMU.liftIO $
                    do putStrLn "Lexer Error:"
                       print $ mkPlainErrMsg flags srcspan msg
    return ()



showToken :: GenLocated SrcSpan Token -> String
showToken t = "srcLoc: " ++ "\n" ++ srcloc ++ "\ntok: " ++ tok
  where
    srcloc = show $ getLoc t
    tok = show $ unLoc t

showTokenWithSource :: (Located Token, String) -> String
showTokenWithSource (loctok, src) =
  "Located Token: " ++
  tok ++ "\n" ++ "Source: " ++ src ++ "\n" ++ "Location: " ++ srcloc ++ "\n\n"
  where
    tok = show $ unLoc loctok
    srcloc = show $ getLoc loctok
