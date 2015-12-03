{--

Copyright (c) 2014-2015, Clockwork Dev Studio
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

module Assembler where          

import Common

import Options
import System.Process
import System.Exit
import System.IO
import System.FilePath.Posix

import Data.Maybe
import Data.List
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Identity

assemble :: CodeTransformation ()
assemble =
  
  do config <- gets asmStateConfig
     code <- gets asmStateCode
     let asmFileName = configAsmFileName config
         objectFileName = configObjectFileName config
         inputFile = configInputFile config
         outputFile = configOutputFile config
         options = configOptions config
         verbose = optionVerbose options
     
     liftIO $ hPutStr outputFile (concat code)
     liftIO $ hClose inputFile
     liftIO $ hClose outputFile

     verboseCommentary ("Assembling...\n") verbose
     verboseCommentary ("(Assembler backend is '" ++ optionAssembler options ++ "')\n") verbose
     
     case optionAssembler options  of
          "fasm" ->
            do verboseCommentary ("fasm " ++ asmFileName ++ "\n") verbose
               (exit, stdout, stderr) <- liftIO $ readProcessWithExitCode "fasm" [asmFileName] ""
               case exit of
                    (ExitFailure n) -> do error (stderr ++ "\nAssembler error (fasm, code " ++ (show n) ++ ").")
                    _ -> return ()

          "nasm" ->

            do 
#if LINUX==1
               let nasmOptions = [asmFileName, "-o", objectFileName, "-f", "elf64"]
#elif WINDOWS==1
               let nasmOptions = [asmFileName, "-o", objectFileName, "-f", "win64"]
#endif
               verboseCommentary ("nasm " ++ (concat (intersperse " " nasmOptions)) ++ "\n") verbose
               (exit, stdout, stderr) <- liftIO $ readProcessWithExitCode "nasm" nasmOptions ""
               case exit of
                    (ExitFailure n) -> do error (stderr ++ "\nAssembler error (nasm, code " ++ (show n) ++ ").")
                    _ -> return ()
          
     put LinkState {linkStateConfig = config}
