{--

Copyright (c) 2014-2020, Clockwork Dev Studio
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met: 

1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer. 
2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution. 

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

--}

{-# LANGUAGE CPP #-}

module Arguments where

import Prelude hiding (catch)
import LexerData
import Common
import Options

import Data.Char

import System.FilePath.Posix
import System.Directory
import System.IO
import Control.Exception
import System.Exit

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Identity
import Debug.Trace

import qualified Data.Sequence as Seq

data ConfigFile =
  ConfigFile
  {
    configFileVariableList :: [ConfigFileVariable]
  } deriving (Show,Eq)

data ConfigFileVariable =
  ConfigFileVariable
  {
    configFileVariableName :: String,
    configFileVariableValue :: String
  } deriving (Show, Eq)

loadConfigFile :: Handle -> ConfigFile -> IO ConfigFile
loadConfigFile handle (ConfigFile variables) =
  do let endOfFile :: IOError -> IO String
         endOfFile e = do return "EOF"
                          
     line <- catch (hGetLine handle) endOfFile
     
     if line == "EOF"
     then return (ConfigFile variables)
     else do let (variable,rest) = span (isAlpha) line
             if variable == [] || rest == [] || head rest /= '='
             then loadConfigFile handle (ConfigFile variables)
             else do loadConfigFile handle (ConfigFile (variables ++ [(ConfigFileVariable variable (tail rest))]))

adjustOptionsBasedOnConfigFile :: Options -> [ConfigFileVariable] -> Options
adjustOptionsBasedOnConfigFile originalOptions (configFileVariable:rest) =
  case configFileVariableName configFileVariable of
       "backend" -> adjustOptionsBasedOnConfigFile (originalOptions {optionAssembler = configFileVariableValue configFileVariable}) rest
       
adjustOptionsBasedOnConfigFile originalOptions _ = originalOptions
  
processArguments :: CodeTransformation ()
processArguments = 
  
  do homeDirectory <- liftIO $ getHomeDirectory
     let writeDefaultConfigFile :: IOError -> IO Handle
         writeDefaultConfigFile _ =
           do newConfHandle <- openFile (homeDirectory ++ "/.idlewild-lang.conf") WriteMode
              hPutStrLn newConfHandle "backend=nasm"
              hClose newConfHandle
              newConfHandle <- openFile (homeDirectory ++ "/.idlewild-lang.conf") ReadMode
              return newConfHandle
     
     confHandle <- liftIO $ (catch (openFile (homeDirectory ++ "/.idlewild-lang.conf") ReadMode) writeDefaultConfigFile)
     configFile <- liftIO $ loadConfigFile confHandle (ConfigFile [])
     let customisedOptions = adjustOptionsBasedOnConfigFile defaultOptions (configFileVariableList configFile)
     
     liftIO $ hClose confHandle
     
     arguments <- gets argumentStateArguments
     (options, nonOptions) <- liftIO $ processOptions customisedOptions arguments

     if optionShowVersion options == True
     then do liftIO $ putStrLn "Idlewild-Lang version 0.0.5."
             liftIO $ exitSuccess
     else return ()

     if length nonOptions /= 1
     then do liftIO $ putStrLn "Please specify one (and only one) source file name."
             liftIO $ exitSuccess
     else return ()

     let sourceFileName = head nonOptions
         asmFileName = replaceExtension sourceFileName ".asm"
#if LINUX==1 || MAC_OS==1
         objectFileName = replaceExtension sourceFileName ".o"
#elif WINDOWS==1
         objectFileName = replaceExtension sourceFileName ".obj"
#endif
         verbose = optionVerbose options
     
     fromHandle <- liftIO $ openFile sourceFileName ReadMode
     toHandle <- liftIO $ openFile asmFileName WriteMode

     code <- liftIO $  hGetContents fromHandle

     put LexState
         {lexStateID = LEX_PENDING,
          lexStateIncludeFileDepth = 0,
          lexStateIncludeFileNameStack = [sourceFileName],
          lexStateIncludeFileNames = [],
          lexStateCurrentToken = emptyToken,
          lexStatePendingTokens = Seq.empty,
          lexStateTokens = Seq.singleton (createBOFToken sourceFileName),
          lexStateLineNumber = 1,
          lexStateLineOffset = 0,
          lexStateCharacters = code,
          lexStateCompoundTokens = allCompoundTokens,
          lexStateConfig = Config {configInputFile = fromHandle,
                                   configOutputFile = toHandle,
                                   configSourceFileName = sourceFileName,
                                   configAsmFileName = asmFileName,
                                   configObjectFileName = objectFileName,
                                   configOptions = options}}

     verboseCommentary ("Program arguments okay...\n") verbose
     verboseCommentary ("Source file '" ++ sourceFileName ++ "'...\n") verbose
