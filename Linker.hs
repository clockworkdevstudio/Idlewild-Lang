{--

Copyright (c) 2014-2017, Clockwork Dev Studio
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

module Linker where          

import Common
import Options

import Data.Maybe
import Data.List
import Data.Char
import System.Process
import System.IO
import System.Exit
import System.FilePath.Posix
import System.Directory

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Identity

import Debug.Trace

link :: CodeTransformation ()
link =
  
  do objectFileName <- gets (configObjectFileName . linkStateConfig)
     rawOutputFileName <- gets (optionOutputFileName . configOptions . linkStateConfig)
     
     libraries <- gets (optionLibraries . configOptions . linkStateConfig)
     verbose <- gets (optionVerbose . configOptions . linkStateConfig)
     
     console <- gets (optionConsole . configOptions . linkStateConfig)

     verboseCommentary ("Linking...\n") verbose
     
#if LINUX==1
  
     let standardLibraryNames =
           if console
           then ["koshka.core"]
           else ["koshka.core","koshka.mm"]
         outputFileName = rawOutputFileName

         gccOptions = [objectFileName,"-o",rawOutputFileName, "-Wl,-rpath=.", "-lc", "-lm", "-lglib-2.0"] ++ (map ("-l" ++) standardLibraryNames) ++ (map ("-l" ++) libraries)
     verboseCommentary ("gcc " ++ (concat (intersperse " " gccOptions)) ++ "\n") verbose
     
     (exit, stdout, stderr) <- liftIO $ readProcessWithExitCode "gcc" gccOptions ""

#elif MAC_OS==1

     let standardLibraryNames =
           if console
           then ["koshka.core"]
           else ["koshka.core","koshka.mm"]
         outputFileName = rawOutputFileName

         gccOptions = ["-o",rawOutputFileName,objectFileName,"-L",".","-L","/usr/local/lib/","-lc", "-lm"] ++ (map ("-l" ++) standardLibraryNames) ++ (map ("-l" ++) libraries)
     verboseCommentary ("gcc " ++ (concat (intersperse " " gccOptions)) ++ "\n") verbose
     
     (exit, stdout, stderr) <- liftIO $ readProcessWithExitCode "gcc" gccOptions ""
#elif WINDOWS==1
     let outputFileName =
           if ".exe" `isSuffixOf` (map toLower rawOutputFileName)
           then rawOutputFileName
           else rawOutputFileName ++ ".exe"
         standardLibraryNames =
           if console
           then ["libkoshka.core.dll"]
           else ["libkoshka.core.dll","libkoshka.mm.dll","user32.dll"]
         goLinkOptions =
           if console
           then [objectFileName,"/entry","main","/fo",outputFileName,"/console"] ++ standardLibraryNames ++ ["msvcrt.dll","kernel32.dll"]
           else [objectFileName,"/entry","main","/fo",outputFileName] ++ standardLibraryNames ++ ["msvcrt.dll","kernel32.dll"]
     --verboseCommentary ("golink " ++ (concat (intersperse " " goLinkOptions)) ++ "\n") verbose
     f <- liftIO $ openFile "link.bat" WriteMode
     liftIO $ hPutStr f ("golink " ++ (concat (intersperse " " goLinkOptions)))
     liftIO $ hClose f
     (exit, stdout, stderr) <- liftIO $ readProcessWithExitCode "link.bat" goLinkOptions ""
#endif

     case exit of
          (ExitFailure n) -> do liftIO $ do k <- getCurrentDirectory
                                            putStrLn (show k)
                                liftIO $ putStr stdout
                                liftIO $ putStrLn (stderr ++ "\nLink error (code " ++ (show n) ++ ").")
                                liftIO $ exitFailure
          _ -> do liftIO $ putStr stdout
                  verboseCommentary ("Successful. Output file: '" ++ outputFileName ++ "'.\n") verbose
