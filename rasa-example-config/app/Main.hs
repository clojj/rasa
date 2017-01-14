{-# language OverloadedStrings, TemplateHaskell #-}
module Main where

import Rasa (rasa)
import Rasa.Ext
import Rasa.Ext.Cursors
import Rasa.Ext.Views
import Rasa.Ext.Files
import Rasa.Ext.Logger
import Rasa.Ext.Slate
import Rasa.Ext.StatusBar
import Rasa.Ext.Style
import Rasa.Ext.Vim

import qualified Control.Lens as L
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
import System.IO

-- import qualified Data.Text as T
-- import Rasa.Internal.Events

import Data.Typeable
import Data.Default

import Control.Monad


newtype RasaChan a = RasaChan (Chan a)

instance Show (RasaChan a) where
  show _ = "Lexer Channel"

newtype ChannelStore = ChannelStore
  {
    _chans  :: (RasaChan String, RasaChan [Located Token])
  } deriving (Show, Typeable)

instance Default ChannelStore where
  def = ChannelStore undefined

L.makeLenses ''ChannelStore  
  
chs :: HasEditor e => L.Lens' e (RasaChan String, RasaChan [Located Token])
chs = ext . chans


-- The @do@ block is of type 'Rasa.Ext.Scheduler.Scheduler'
main :: IO ()
main =
  rasa $ do
    _ <- onInit initLexer
    views
    vim
    statusBar
    files
    cursors
    logger
    slate
    -- eventListener lexBuf
    _ <- beforeRender lexIt
    style
    void $ newBuffer "This is a buffer to get you started!\nYou can also pass command line args to rasa"

-- TODO async tokenization ?

-- TODO complete issue #20, then do some optimized tokenization...
-- lexBuf :: BufTextChanged -> Action ()
-- lexBuf (BufTextChanged chg) = lexHaskell ch

lexIt :: Action ()
lexIt = do
  (RasaChan chIn, RasaChan chOut) <- L.use chs
  lexHaskell chIn chOut
  return ()

lexHaskell :: Chan String -> Chan [Located Token] -> Action ()
lexHaskell chIn chOut = 
  void $ focusDo $ do
    src <- L.use (text . asString)
    liftIO $ do
      writeChan chIn src
      tokens <- readChan chOut
      -- TODO tokens to styles! stderr is just for testing !
      hPutStr stderr $ "TOKENS\n" ++ concatMap showToken tokens
      return ()

initLexer :: Action ()
initLexer = do
  channels <- liftIO startGhc
  ext L..= channels
  return ()

startGhc :: IO ChannelStore
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
  return $ ChannelStore (RasaChan chIn, RasaChan chOut)

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
