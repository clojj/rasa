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

import Control.Monad
import Control.Concurrent
import System.IO
import Data.Typeable
import Data.Default


newtype RasaMVar a = RasaMVar (MVar a)

instance Show (RasaMVar a) where
  show _ = "Show MVar"

newtype MVarStore = MVarStore
  {
    _chans  :: (RasaMVar String, RasaMVar [Located Token])
  } deriving (Show, Typeable)

instance Default MVarStore where
  def = MVarStore undefined

L.makeLenses ''MVarStore  
  
chs :: HasEditor e => L.Lens' e (RasaMVar String, RasaMVar [Located Token])
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
    _ <- onBufTextChanged lexIt
    style
    void $ newBuffer "module Test where\n\nmain = do\n  line <- getLine\n  print line"

-- TODO async tokenization ?

-- TODO complete issue #20, then do some optimized tokenization by using range
lexIt :: Range -> Action ()
lexIt r = do
  (RasaMVar chIn, RasaMVar chOut) <- L.use chs
  lexHaskell chIn chOut
  printChg r
  return ()

lexHaskell :: MVar String -> MVar [Located Token] -> Action ()
lexHaskell chIn chOut = 
  void $ focusDo $ do
    src <- L.use (getText . asString)
    liftIO $ do
      putMVar chIn src

      -- TODO delayed lexing works only when async delivery
      tokens <- takeMVar chOut

      -- TODO tokens to styles! stderr is just for testing !
      hPutStr stderr $ "TOKENS\n" ++ concatMap showToken tokens

      return ()

initLexer :: Action ()
initLexer = do
  channels <- liftIO startGhc
  ext L..= channels
  return ()

startGhc :: IO MVarStore
startGhc = do
  chIn <- newEmptyMVar
  chOut <- newEmptyMVar
  threadId <- forkIO $ runGhc (Just libdir) $ do
    flags <- getSessionDynFlags
    let lexLoc = mkRealSrcLoc (mkFastString "<interactive>") 1 1
    GMU.liftIO $ forever $ do
        str <- takeMVar chIn

        -- TODO delayed lexing works only when async delivery
{-        
        Control.Concurrent.threadDelay 1000000
        newInput <- tryTakeMVar chIn
        str' <- case newInput of
                  Nothing -> return str
                  Just newStr -> do
                    hPutStr stderr "\nDELAYED\n"
                    return newStr
        let sb = stringToStringBuffer str'
-}
        let sb = stringToStringBuffer str
        let pResult = lexTokenStream sb lexLoc flags
        case pResult of

          POk _ toks -> GMU.liftIO $ do
            -- hPutStr stderr $ "TOKENS\n" ++ concatMap showToken toks
            -- TODO delayed lexing works only when async delivery
            putMVar chOut toks

          PFailed srcspan msg -> do
            GMU.liftIO $ print $ show srcspan
            GMU.liftIO $
              do putStrLn "Lexer Error:"
                 print $ mkPlainErrMsg flags srcspan msg
  return $ MVarStore (RasaMVar chIn, RasaMVar chOut)

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

printChg :: Range -> Action ()
printChg r = logInfo $ "BufTextChanged " ++ show r
