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
import qualified Data.Text as T
import System.IO

-- import Data.Default
-- import Rasa.Internal.Events


-- | This is the main of an executable that runs rasa with any extensions the
-- user wants
--
-- The @do@ block is of type 'Rasa.Ext.Scheduler.Scheduler'
main :: IO ()
main = do
  (chIn, chOut) <- startGhc
  rasa $ do
     vim
     statusBar
     files
     cursors
     logger
     slate
    --  eventListener lexBuf
     beforeRender $ lexHaskell chIn chOut
     style



-- TODO async tokenization ?

-- TODO complete issue #20, then do some optimized tokenization...
-- lexBuf :: BufTextChanged -> Action ()
-- lexBuf (BufTextChanged chg) = lexHaskell ch

lexHaskell :: Chan String -> Chan [Located Token] -> Action ()
lexHaskell chIn chOut = 
  focusDo $ do
    src <- use text
    liftIO $ do
      writeChan chIn (T.unpack src)
      tokens <- readChan chOut
      hPutStr stderr $ "TOKENS\n" ++ concatMap showToken tokens
      return ()


startGhc :: IO (Chan String, Chan [Located Token])
startGhc = do
  chIn <- newChan
  chOut <- newChan
  threadId <- forkIO $ runGhc (Just libdir) $ do
    flags <- getSessionDynFlags
    let lexLoc = mkRealSrcLoc (mkFastString "<interactive>") 1 1
    GMU.liftIO $ forever $ do
        str <- readChan chIn
        let sb = stringToStringBuffer str
        let pResult = lexTokenStream sb lexLoc flags
        case pResult of
          POk _ toks -> GMU.liftIO $ writeChan chOut toks
          PFailed srcspan msg -> do
            GMU.liftIO $ print $ show srcspan
            GMU.liftIO $
              do putStrLn "Lexer Error:"
                 print $ mkPlainErrMsg flags srcspan msg
  return (chIn, chOut)

showToken :: GenLocated SrcSpan Token -> String
showToken t = "\nsrcLoc: " ++ srcloc ++ "\ntok: " ++ tok ++ "\n"
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
