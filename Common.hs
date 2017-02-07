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

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Common where

import LexerData
import ParserData
import SemanticsData
import CompilerData

import Options

import qualified Data.Map as Map
import qualified Data.Sequence as Seq

import System.IO
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Identity
import Control.Applicative (Applicative(..))

instance Functor CodeTransformation where
    fmap = liftM

instance Applicative CodeTransformation where
    pure  = return
    (<*>) = ap

data Config =
     Config
     {
       configInputFile :: Handle,
       configOutputFile :: Handle,
       configSourceFileName :: String,
       configAsmFileName :: String,
       configObjectFileName :: String,
       configOptions :: Options
     } deriving (Show)

data CodeState =
     ArgumentState
     {
       argumentStateArguments :: [String]
     } |
     LexState
     {
       lexStateID :: LexStateID,
       lexStateIncludeFileDepth :: Int,
       lexStateIncludeFileNameStack :: [String],
       lexStateCurrentToken :: Token,
       lexStatePendingTokens :: Seq.Seq Token,
       lexStateTokens :: Seq.Seq Token,
       lexStateLineNumber :: Int,
       lexStateLineOffset :: Int,
       lexStateCharacters :: String,
       lexStateCompoundTokens :: [CompoundToken],
       lexStateConfig :: Config
     } |
     ParseState
     {
       parseStateIncludeFileNameStack :: [String],
       parseStateTree :: Statement,
       parseStateTokens :: [Token],
       parseStateInFunction :: Bool,
       parseStateConfig :: Config
     } |
     SemanticState
     {
       semanticStateIncludeFileNameStack :: [String],
       semanticStateProgram :: [Statement],
       semanticStateSymbols :: SymbolTable,
       semanticStateLocalSymbols :: SymbolTable,
       semanticStateTypes :: SymbolTable,
       semanticStateInts :: IntConstantTable,
       semanticStateFloats :: FloatConstantTable,
       semanticStateStrings :: StringTable,
       semanticStateNameSpace :: Symbol,
       semanticStateLoopDepth :: Int,
       semanticStateConfig :: Config
     } |
     CompileState
     {
       compileStateIncludeFileNameStack :: [String],
       compileStateProgram :: [Statement],
       compileStateAsm :: Asm,
       compileStateSymbols :: SymbolTable,
       compileStateLocalSymbols :: SymbolTable,
       compileStateTypes :: SymbolTable,
       compileStateInts :: IntConstantTable,
       compileStateFloats :: FloatConstantTable,
       compileStateStrings :: StringTable,
       compileStateNameSpace :: Symbol,
       compileStateExitLabelIDs :: [Int],
       compileStateRegisters :: CPUContext,
       compileStateLineNumber :: Int,
       compileStateLabelID :: Int,
       compileStateConfig :: Config
     } |
     AsmState
     {
       asmStateCode :: [String],
       asmStateConfig :: Config
     } |
     LinkState
     {
       linkStateConfig :: Config
     }
     deriving (Show)

newtype CodeTransformation action =
        CodeTransformation
        {runCodeTransformation :: ErrorT FatalError (StateT CodeState IO) action}
        deriving (Monad, MonadIO, MonadError FatalError, MonadState CodeState)

codeTransformation :: CodeState -> CodeTransformation () -> IO (Either FatalError (), CodeState)
codeTransformation codeState action = (runStateT (runErrorT (runCodeTransformation action))) codeState

transformCode :: CodeTransformation () -> CodeState -> IO CodeState
transformCode action state =
  do (success,codeState) <- codeTransformation state action
     case success of
          (Left fatalError) -> error (printFatalError fatalError)
          (Right ()) -> return codeState               

verboseCommentary :: String -> Bool -> CodeTransformation ()
verboseCommentary msg verbose =
  do if verbose
     then liftIO $ putStr msg                
     else return ()

instance Error FatalError

data FatalError =
     FatalError
      {
        fatalErrorFileName :: String,
        fatalErrorLineNumber :: Int,
        fatalErrorOffset :: Int,
        fatalErrorMessage :: String
      }
     deriving (Show, Eq)

printFatalError :: FatalError -> String
printFatalError (FatalError fileName line offset msg) =
    fileName ++ ":" ++ show line ++ ":" ++ show offset ++ ": " ++ msg

