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

module Options where

import System.Console.GetOpt
import System.FilePath.Posix
import Data.Maybe (fromMaybe)

data Options = Options
         {optionVerbose :: Bool,
          optionConsole :: Bool,
          optionShowVersion :: Bool,
          optionOutputFileName :: FilePath,
          optionLibraries :: [String],
          optionAssembler :: String,
          optionDebug :: Bool,
          optionAbstractSyntaxTree :: Bool,
          optionGosubDepth :: Int
         } deriving Show

defaultOptions = Options
        {optionVerbose = False,
         optionConsole = False,
         optionShowVersion = False,
#if LINUX==1
         optionOutputFileName = "a.out",
#elif MAC_OS==1
         optionOutputFileName = "a.out",
#elif WINDOWS==1
         optionOutputFileName = "a.exe",
#endif
         optionLibraries = [],
         optionAssembler = "nasm",
         optionDebug = False,
         optionAbstractSyntaxTree = False,
         optionGosubDepth = 256
        }

options :: [OptDescr (Options -> Options)]
options =
        [Option ['v'] ["verbose"]
         (NoArg (\ opts -> opts { optionVerbose = True})) 
                  "Show detailed output.",
         Option ['c'] ["console"]
                  (NoArg (\ opts -> opts { optionConsole = True})) 
                  "Create a console-only (and non-multimedia) executable with minimal dependencies.",
         Option ['V', '?'] ["version"]
                  (NoArg (\ opts -> opts { optionShowVersion = True }))
                  "Show the compiler version number.",
         Option ['o'] ["output"]
                  (ReqArg ((\ f opts -> opts { optionOutputFileName = f}))
                   "FILE")
                   "Destination file name (default: a.out [Linux]; a.exe [Windows]).",
         Option ['l'] ["lib"]
                  (ReqArg addLibraryName
                   "LIB")
                   "Additional system library to link against.",         
         Option ['b'] ["backend"]
                  (ReqArg ((\ f opts -> opts { optionAssembler = f }))
                   "ASSEMBLER")
                   "Assembler backend (default: nasm [unless otherwise specified in ~/.idlewild-lang.conf]).",
         Option ['d'] ["debug"]
                  (NoArg (\ opts -> opts { optionDebug = True }))
                  "Include DWARF debugging information and diagnostic messages.",
         Option ['a'] ["abstract"]
                  (NoArg (\ opts -> opts { optionAbstractSyntaxTree = True }))
                  "Show abstract syntax tree.",
         Option ['g'] ["max-gosub-depth"]
                  (ReqArg ((\f opts -> opts {optionGosubDepth = (read f :: Int)}))
                   "GOSUB_DEPTH")
                   "Maximum number of nested Gosubs."]

addLibraryName :: String -> Options -> Options
addLibraryName name options =
  options {optionLibraries = optionLibraries options ++ [name]}

processOptions :: Options -> [String] -> IO (Options, [String])
processOptions customisedOptions argv =
        case getOpt Permute options argv of
             (opts,nonOpts,[]) -> return (foldl (flip id) customisedOptions opts, nonOpts)
             (_,_,errors) -> ioError (userError (concat errors ++ usageInfo quickHelp options))
        where quickHelp = "Usage: idlewild-lang [OPTION...] filename..."
