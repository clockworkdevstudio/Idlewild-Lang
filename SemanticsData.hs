{--

Copyright (c) 2014-2016, Clockwork Dev Studio
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

module SemanticsData where

import ParserData

import qualified Data.Map as Map

{--
data Reference =
     LabelReference |
     TypeReference |
     FunctionReference
     {
       functionReferenceType :: VariableType,
       functionReferenceArgumentTypes :: [VariableType],
       functionReferenceMinNumArguments :: Int,
       functionReferenceMaxNumArguments :: Int
     }
     deriving (Show, Eq)
--}

data Parameter =
     Parameter
     {
       parameterName :: String,
       parameterType :: VariableType,
       parameterDefaultValue :: Statement
     }
     deriving (Show, Eq)

data Symbol =
     Variable
     {
       variableName :: String,
       variableType :: VariableType,
       variableIsGlobal :: Bool
     } |
     LocalAutomaticVariable
     {
       localAutomaticVariableName :: String,
       localAutomaticVariableType :: VariableType,
       localAutomaticVariableIsArgument :: Bool,
       localAutomaticVariableAddress :: Int
     } |
     Const
     {
       constName :: String,
       constType :: VariableType,
       constValue :: ConstValue
     } |
     Array
     {
       arrayName :: String,
       arrayType :: VariableType,
       arrayNumDimensions :: Int
     } |
     Label
     {
       labelName :: String
     } |
     Function
     {
       functionName :: String,
       functionRawName :: String,
       functionType :: VariableType,
       functionOrigin :: FunctionOrigin,
       functionParameters :: [Parameter],
       functionMaxNumArguments :: Int,
       functionMinNumArguments :: Int,
       functionNumRegisterArguments :: Int,
       functionNumStackArguments :: Int,
       functionNumLocals :: Int,
       functionUsed :: Bool,
       functionSymbols :: SymbolTable
     } |
     Type
     {
       typeName :: String,
       typeSize :: Int,
       typeSymbols :: SymbolTable
     } |
     Field
     {
       fieldName :: String,
       fieldType :: VariableType,
       fieldOffset :: Int
     } |
     NO_NAMESPACE
     deriving (Show, Eq)

data ConstValue =
  ConstValue
  {
    constValueExpression :: Statement
  }
  deriving (Show,Eq)

data FunctionOrigin =
     FUNCTION_ORIGIN_STANDARD |
     FUNCTION_ORIGIN_SYSTEM |
     FUNCTION_ORIGIN_USER
     deriving (Show,Eq)

type SymbolTable = Map.Map String Symbol
{--type ForwardReferenceTable = Map.Map String Symbol--}
type StringTable = Map.Map String Int
type IntConstantTable = Map.Map Int Int
type FloatConstantTable = Map.Map Double Int

