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
{-# LANGUAGE CPP #-}
                                                 
module Compiler where

import LexerData
import ParserData
import SemanticsData
import CompilerData

import Lexer
import Parser
import Semantics

import Common
import Options

import DWARF

import System.IO
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Identity
import Debug.Trace

import Data.List
import Data.Ord
import Data.Word
import Data.Char
import Data.Bits
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Foldable as Foldable

putAsm :: [String] -> CodeTransformation ()
putAsm instructions =
        do state <- get

           let (Asm bucketName bucketMap) = compileStateAsm state
               modifiedBucketMap = Map.adjust (modifyAsmBucket instructions) bucketName bucketMap
           put state {compileStateAsm = Asm bucketName modifiedBucketMap}

putAsmGeneric :: [String] -> [String] -> CodeTransformation ()
putAsmGeneric fasmInstructions nasmInstructions =
        do state <- get
           let (Asm bucketName bucketMap) = compileStateAsm state
               backend = (optionAssembler . configOptions . compileStateConfig) state
               modifiedBucketMap =
                 case backend of
                      "fasm" ->
                        Map.adjust (modifyAsmBucket fasmInstructions) bucketName bucketMap
                      "nasm" ->
                        Map.adjust (modifyAsmBucket nasmInstructions) bucketName bucketMap
                      k -> error (show k)
           put state {compileStateAsm = Asm bucketName modifiedBucketMap}

modifyAsmBucket :: [String] -> AsmBucket -> AsmBucket
modifyAsmBucket instructions (AsmBucket bucketContents) =

        AsmBucket (bucketContents Seq.|> (concat instructions))

putNewline :: CodeTransformation ()
putNewline =
        do putAsm ["\n"]

compile :: CodeTransformation ()
compile =

        do verbose <- gets (optionVerbose . configOptions . compileStateConfig)
           verboseCommentary ("Compiling...\n") verbose
           (Statement {statementContents = program}:_) <- gets compileStateProgram
           initialiseCompiler
           putAsmFileInfo
           putDirectivesAndFunctionDeclarations
           putGlobalData
           
           {--
           b <- getAsmBucket
           setAsmBucket data_
           putAsm
             ["SECTION .data\nBASIC_DATA:\n\n"]
           putDataSentinel
           setAsmBucket b
           --}
           
           putAuxiliaryCode
           
           compileProgram program

           putInts
           putFloats
           putStrings
           putEachTypeStringOffsets
           putCleanup
           putEndFunction
           
           putDebugInfo
           
           asm <- gets compileStateAsm
           config <- gets compileStateConfig
           
           let options = configOptions config
               text = (concatMap (Foldable.toList . asmBucketContents . snd) (Map.toAscList (asmBucketMap asm)))
           
           put (AsmState text config)

initialiseCompiler :: CodeTransformation ()
initialiseCompiler =
  do sourceFileName <- gets (configSourceFileName . compileStateConfig)
     addRuntimeErrorString ("\"" ++ sourceFileName ++ "\"")
     addRuntimeErrorString runtimeErrorOnGotoRangeException
     addRuntimeErrorString runtimeErrorNullPointerException
     addRuntimeErrorString runtimeErrorDivisionByZeroException
           
putAsmFileInfo :: CodeTransformation ()
putAsmFileInfo =
  do bucket <- getAsmBucket
     setAsmBucket id_
     putAsm
       ["; Idlewild-Lang output "]
     putAsmGeneric
       ["(fasm syntax)\n\n"]
       ["(nasm syntax)\n\n"]
     setAsmBucket bucket

compileProgram :: [Statement] -> CodeTransformation ()
compileProgram (codeStatement : _) =

        do bucket <- getAsmBucket
           setAsmBucket code_
           
           putAsmGeneric ["section '.text' executable\n\n"] ["SECTION .text\n\n"]

#if LINUX == 1 || MAC_OS==1
           putAsm
             [osFunctionPrefix ++ "main:\n\n",
              "push rbp\n",
              "mov rbp, rsp\n",
              "push rbx\n",
              "push r12\n",
              "push r13\n",
              "push r14\n",
              "push r15\n\n"]
#elif WINDOWS == 1
           putAsm
             [osFunctionPrefix ++ "main:\n\n",
              "push rbp\n",
              "mov rbp, rsp\n",
              "push rbx\n",
              "push rsi\n",
              "push rdi\n",
              "push r12\n",
              "push r13\n",
              "push r14\n",
              "push r15\n"]
#endif

           putStandardLibraryInit
           putLinkedListInit

           compileCode codeStatement

           putLinkedListFinal
           putStandardLibraryFinal

           setAsmBucket bucket

putStandardLibraryInit :: CodeTransformation ()
putStandardLibraryInit =
  do maxGosubDepth <- gets (optionGosubDepth . configOptions . compileStateConfig)
     console <- gets (optionConsole . configOptions . compileStateConfig)
     let initFunctionName =
           if console
           then "init_libkoshka_core"
           else "init_libkoshka_mm"
     r1 <- reserveRegisterForFunctionCallWithPreference (functionCallRegisters !! 0)
     putAsm
       ["mov " ++ registerName r1 ++ ", " ++ show maxGosubDepth ++ "\n"]
     insertFunctionCall initFunctionName
       [RawRegister (registerName r1) VARIABLE_TYPE_INT]
     resetRegisterState

#if WINDOWS==1
     putAsm
       ["push rax\n",
        "mov rcx, 2\n",
        "mov rax, 0\n",
        "call [dpiHack wrt ..got]\n",
        "pop rax\n"]
#else
     putAsm
       ["push rax\n",
        "mov rax, 0\n",
        "call [" ++ osFunctionPrefix ++ "dpiHack wrt ..got]\n",
        "pop rax\n"]
#endif

putStandardLibraryFinal =
  do console <- gets (optionConsole . configOptions . compileStateConfig)
     let finalFunctionName =
           if console
           then "final_libkoshka_core"
           else "final_libkoshka_mm"
  
     insertFunctionCall finalFunctionName []
     putAsm
       ["sub rsp, 8\n",
        "call bbu_end\n"]
     resetRegisterState

compilerPushIncludeFileName :: String -> CodeTransformation ()
compilerPushIncludeFileName fileName =
  do state <- get
     put $ state {compileStateIncludeFileNameStack =
                  (fileName:compileStateIncludeFileNameStack state)}

compilerPopIncludeFileName :: CodeTransformation ()
compilerPopIncludeFileName =
  do state <- get
     put $ state {compileStateIncludeFileNameStack =
                  tail (compileStateIncludeFileNameStack state)}

compileCode :: Statement -> CodeTransformation ()
compileCode (Statement {statementContents = statements})

        | statements == [] =
            do return ()

        | otherwise =

            do compileFunc statement
               compileCode (createMinimalStatement STATEMENT_CODE (tail statements))

        where statement = head statements
              compileFunc = case statementID statement of
                                 STATEMENT_BEGINNING_OF_FILE -> compileBeginningOfFile
                                 STATEMENT_END_OF_FILE -> compileEndOfFile
                                 STATEMENT_CODE -> compileCode
                                 STATEMENT_NEWLINE -> compileNewLine
                                 STATEMENT_DIM -> compileDimStatement
                                 STATEMENT_END_STATEMENT -> compileEndStatement
                                 STATEMENT_COMMENT -> compileComment
                                 STATEMENT_ASM -> compileAsm
                                 STATEMENT_ASM_DATA -> compileAsmData
                                 STATEMENT_SYS -> compileSys
                                 STATEMENT_EXPRESSION -> compileTopLevelExpression
                                 STATEMENT_DATA -> compileData
                                 STATEMENT_READ -> compileRead
                                 STATEMENT_RESTORE -> compileRestore
                                 STATEMENT_FOR -> compileFor
                                 STATEMENT_FOR_EACH -> compileForEach
                                 STATEMENT_WHILE -> compileWhile
                                 STATEMENT_REPEAT -> compileRepeat
                                 STATEMENT_EXIT -> compileExit
                                 STATEMENT_IF -> compileIf
                                 STATEMENT_SELECT -> compileSelect
                                 STATEMENT_LABEL -> compileLabel
                                 STATEMENT_DATA_LABEL -> compileDataLabel
                                 STATEMENT_ON_GOTO -> compileOnGoto
                                 STATEMENT_ON_GOSUB -> compileOnGosub
                                 STATEMENT_GOTO -> compileGoto
                                 STATEMENT_GOSUB -> compileGosub
                                 STATEMENT_RETURN -> compileReturn
                                 STATEMENT_MULTI_FUNCTION -> compileMultiFunction
                                 STATEMENT_MULTI_FUNCTION_RETURN -> compileMultiFunctionReturn
                                 STATEMENT_CONST -> compileConst
                                 STATEMENT_GLOBAL -> compileGlobal
                                 STATEMENT_LOCAL -> compileLocal
                                 STATEMENT_END -> compileEnd
                                 STATEMENT_TYPE -> compileNothing
                                 STATEMENT_INSERT_BEFORE -> compileInsert
                                 STATEMENT_INSERT_AFTER -> compileInsert
                                 STATEMENT_DELETE -> compileDelete
                                 STATEMENT_DELETE_EACH -> compileDeleteEach
                                 STATEMENT_NONE -> doNothing
                                 _ -> error ("Critical error in function compileCode.")

              doNothing _  = do return ()

compileNothing :: Statement -> CodeTransformation ()
compileNothing _ = do return ()

compileInsert :: Statement -> CodeTransformation ()
compileInsert (Statement {statementID = id,
                            statementContents = (object:expression:_)}) =

  do symbols <- gets compileStateSymbols
     localSymbols <- gets compileStateLocalSymbols
     types <- gets compileStateTypes
     
     let objectType = getExpressionType object symbols localSymbols types
         expressionType = getExpressionType expression symbols localSymbols types
         name = map toLower (customTypeName objectType)

     setDataType objectType
     compileExpression object
     leftRegister <- gets currentRegister
     
     setDataType expressionType
     compileExpression expression
     rightRegister <- gets currentRegister

     r1 <- reserveRegisterForFunctionCallWithPreference (functionCallRegisters !! 0)
     r2 <- reserveRegisterForFunctionCallWithPreference (functionCallRegisters !! 1)
     r3 <- reserveRegisterForFunctionCallWithPreference (functionCallRegisters !! 2)

     putAsm
       ["mov " ++ registerName r1 ++ ", [" ++ decorateLinkedListName name ++ "]\n",
        "mov " ++ registerName r2 ++ ", " ++ registerName leftRegister ++ "\n",
        "mov " ++ registerName r3 ++ ", " ++ registerName rightRegister ++ "\n"]
     
     insertFunctionCall insertFunctionName
       [RawRegister (registerName r1) VARIABLE_TYPE_INT,
        RawRegister (registerName r2) VARIABLE_TYPE_INT,
        RawRegister (registerName r3) VARIABLE_TYPE_INT]

     deallocateRegisters [leftRegister,rightRegister]

  where insertFunctionName =
          case id of
            STATEMENT_INSERT_BEFORE -> "insert_before"
            STATEMENT_INSERT_AFTER -> "insert_after"

compileDelete :: Statement -> CodeTransformation ()
compileDelete (Statement {statementContents = (expression:_)}) =

  do symbols <- gets compileStateSymbols
     localSymbols <- gets compileStateLocalSymbols
     types <- gets compileStateTypes
     
     let dataType = getExpressionType expression symbols localSymbols types
         name = map toLower (customTypeName dataType)

     setDataType dataType
     compileExpression expression
     result <- gets currentRegister

     r1 <- reserveRegisterForFunctionCallWithPreference (functionCallRegisters !! 0)
     r2 <- reserveRegisterForFunctionCallWithPreference (functionCallRegisters !! 1)
     
     putAsm
       ["mov " ++ registerName r1 ++ ", [" ++ decorateLinkedListName name ++ "]\n",
        "mov " ++ registerName r2 ++ ", " ++ registerName result ++ "\n"]

     insertFunctionCall "delete"
       [RawRegister (registerName r1) VARIABLE_TYPE_INT,
        RawRegister (registerName r2) VARIABLE_TYPE_INT]

     deallocateRegisters [result]

compileDeleteEach :: Statement -> CodeTransformation ()
compileDeleteEach (Statement {statementID = STATEMENT_DELETE_EACH,
                            statementContents = (Statement {statementContents = typeName:_}:_)}) =

  do symbols <- gets compileStateSymbols
     localSymbols <- gets compileStateLocalSymbols
     types <- gets compileStateTypes
     
     let sourceName = identifierExpressionValue (getInitialStatement typeName)
         name = map toLower sourceName
         type_ = Map.lookup name types
                 
     r1 <- reserveRegisterForFunctionCallWithPreference (functionCallRegisters !! 0)
     
     putAsm
       ["mov " ++ registerName r1 ++ ", [" ++ decorateLinkedListName name ++ "]\n"]

     insertFunctionCall "delete_each"
       [RawRegister (registerName r1) VARIABLE_TYPE_INT]

compileBeginningOfFile :: Statement -> CodeTransformation ()
compileBeginningOfFile (Statement {statementContents = (IdentifierExpression fileName:_)}) =
  do compilerPushIncludeFileName fileName

compileEndOfFile :: Statement -> CodeTransformation ()
compileEndOfFile _ =
  do compilerPopIncludeFileName
     
compileNewLine :: Statement -> CodeTransformation ()
compileNewLine _ = do resetRegisterState
                      compilerIncrementLineNumber

compileDimStatement :: Statement -> CodeTransformation ()
compileDimStatement (Statement {statementContents = expressionList}) =
        
        do mapM_ compileDim expressionList

compileDim :: Statement -> CodeTransformation ()
compileDim arrayDeclaration =

        do symbols <- gets compileStateSymbols
           localSymbols <- gets compileStateLocalSymbols
           nameSpace <- gets compileStateNameSpace
           lineNumber <- gets compileStateLineNumber

           let (identifier,dimensions) = getFunctionCallOperands (statementContents (getInitialStatement arrayDeclaration))
               sourceName = identifierExpressionValue (getInitialStatement identifier)
               name = removeTypeTag (map toLower sourceName)
               (Array {arrayType = (VARIABLE_TYPE_ARRAY arrayType)}) = lookupVariable name symbols localSymbols nameSpace
           
           if arrayType == VARIABLE_TYPE_STRING
#if LINUX==1 || MAC_OS==1
           then do r1 <- reserveRegisterForFunctionCallWithPreference rdi
                   r2 <- reserveRegisterForFunctionCallWithPreference rsi
                   r3 <- reserveRegisterForFunctionCallWithPreference rdx
                   
                   putAsm
                     ["mov " ++ registerName r1 ++ ", " ++ decorateVariableName name symbols localSymbols nameSpace ++ "\n",
                      "mov " ++ registerName r2 ++ ", " ++ bbFunctionPrefix ++ "init_string\n",
                      "mov " ++ registerName r3 ++ ", " ++ bbFunctionPrefix ++ "free_string\n"]

#elif WINDOWS==1
           then do r1 <- reserveRegisterForFunctionCallWithPreference rcx
                   r2 <- reserveRegisterForFunctionCallWithPreference rdx
                   r3 <- reserveRegisterForFunctionCallWithPreference r8
                   
                   putAsm
                     ["mov " ++ registerName r1 ++ ", " ++ decorateVariableName name symbols localSymbols nameSpace ++ "\n",
                      "mov " ++ registerName r2 ++ ", [" ++ bbFunctionPrefix ++ "init_string]\n",
                      "mov " ++ registerName r3 ++ ", [" ++ bbFunctionPrefix ++ "free_string]\n"]
#endif

                   insertFunctionCall "allocate_array"
                    ([RawRegister (registerName r1) VARIABLE_TYPE_INT,
                      RawRegister (registerName r2) VARIABLE_TYPE_INT,
                      RawRegister (registerName r3) VARIABLE_TYPE_INT,
                      createMinimalStatement EXPRESSION_INT_CONSTANT [IntConstantExpression 8],
                      createMinimalStatement EXPRESSION_INT_CONSTANT [IntConstantExpression (length dimensions)]] ++ dimensions)

           else do r1 <- reserveRegisterForFunctionCallWithPreference (functionCallRegisters !! 0)
                   putAsm
                     ["mov " ++ registerName r1 ++ ", " ++ decorateVariableName name symbols localSymbols nameSpace ++ "\n"]

                   insertFunctionCall "allocate_array"
                    ([RawRegister (registerName r1) VARIABLE_TYPE_INT,
                      createMinimalStatement EXPRESSION_INT_CONSTANT [IntConstantExpression 0],
                      createMinimalStatement EXPRESSION_INT_CONSTANT [IntConstantExpression 0],
                      createMinimalStatement EXPRESSION_INT_CONSTANT [IntConstantExpression 8],
                      createMinimalStatement EXPRESSION_INT_CONSTANT [IntConstantExpression (length dimensions)]] ++ dimensions)

           resetRegisterState

compileEndStatement :: Statement -> CodeTransformation ()
compileEndStatement _ = do resetRegisterState

compileComment :: Statement -> CodeTransformation ()
compileComment _ = return ()

compileAsm :: Statement -> CodeTransformation ()
compileAsm (Statement {statementContents = (InlineAsm asm:_)}) =
  do putAsm ["\n; Inline assembly:\n\n"]
     putAsm asm
     putAsm ["\n\n"]

compileAsmData :: Statement -> CodeTransformation ()
compileAsmData (Statement {statementContents = (InlineAsm asm:_)}) =
  do bucket <- getAsmBucket
     setAsmBucket globals_
     putAsm ["\n; Inline assembly:\n\n"]
     putAsm ["align 8\n"]
     putAsm asm
     putAsm ["\nalign 8\n\n"]
     setAsmBucket bucket

compileSys :: Statement -> CodeTransformation ()
compileSys _ = do return ()

compileTopLevelExpression :: Statement -> CodeTransformation ()
compileTopLevelExpression (Statement {statementContents = (statement:_)}) =
        do if expressionIsFunctionCall statement
           then do compileTopLevelFunctionCall statement
                   register <- gets currentRegister
                   symbols <- gets compileStateSymbols
                   localSymbols <- gets compileStateLocalSymbols
                   nameSpace <- gets compileStateNameSpace
                   types <- gets compileStateTypes
                   if isLeakyExpression symbols localSymbols types nameSpace statement
                   then do r1 <- reserveRegisterForFunctionCallWithPreference (functionCallRegisters !! 0)
                           putAsm
                             ["mov " ++ registerName r1 ++ ", " ++ registerName register ++ "\n"]
                           insertFunctionCall "free_string"
                             [RawRegister (registerName r1) VARIABLE_TYPE_STRING]
                   else return ()
           else do compileReducedExpression statement

compileTopLevelFunctionCall (Statement {statementID = EXPRESSION_FUNCTION_CALL, 
                                     statementContents = (operand:arguments)}) =

        do dataType <- gets getDataType
           symbols <- gets compileStateSymbols
           localSymbols <- gets compileStateLocalSymbols

           let (IdentifierExpression sourceName) = getInitialStatement operand
               name = removeTypeTag (map toLower sourceName)
               symbol = lookupVariable name symbols Map.empty NO_NAMESPACE
               parameters = functionParameters symbol
           
           insertFunctionCall name (arguments ++ populateDefaultValues parameters)

populateDefaultValues :: [Parameter] -> [Statement]
populateDefaultValues parameters =
  map parameterToDefaultExpression (dropWhile (\p -> parameterDefaultValue p == EmptyStatement) parameters)

parameterToDefaultExpression :: Parameter -> Statement
parameterToDefaultExpression parameter =
  let expression = parameterDefaultValue parameter
      intValue = intConstantExpressionValue (getInitialStatement expression)
      floatValue = floatConstantExpressionValue (getInitialStatement expression)
      stringValue = stringConstantExpressionValue (getInitialStatement expression) in
  case statementID expression of
    EXPRESSION_INT_CONSTANT ->
      createMinimalStatement EXPRESSION_INT_CONSTANT [IntConstantExpression intValue]
    EXPRESSION_FLOAT_CONSTANT ->
      createMinimalStatement EXPRESSION_FLOAT_CONSTANT [FloatConstantExpression floatValue]
    EXPRESSION_STRING_CONSTANT ->
      createMinimalStatement EXPRESSION_STRING_CONSTANT [StringConstantExpression stringValue]
    EXPRESSION_NULL ->
      createMinimalStatement EXPRESSION_NULL [NullExpression]

compileExpression :: Statement -> CodeTransformation ()
compileExpression statement =
        do symbols <- gets compileStateSymbols
           if expressionIsConstant symbols statement
           then do localSymbols <- gets compileStateLocalSymbols
                   compileReducedExpression (reduceConstantExpression statement symbols localSymbols)
           else do compileReducedExpression statement

compileData :: Statement -> CodeTransformation ()
compileData (Statement {statementContents = constantList}) =

        do bucket <- getAsmBucket
           setAsmBucket data_
           mapM_ putData constantList
           setAsmBucket bucket

putData :: Statement -> CodeTransformation ()
putData statement =

        do symbols <- gets compileStateSymbols
           localSymbols <- gets compileStateLocalSymbols
           strings <- gets compileStateStrings
           let reducedExpression = reduceConstantExpression statement symbols localSymbols

           case reducedExpression of
             
                (Statement {statementID = EXPRESSION_INT_CONSTANT,  
                            statementContents = (IntConstantExpression value:_)}) ->

                  do putAsm
                       ["dq DATA_TYPE_INT, " ++ show value ++ "\n"]

                (Statement {statementID = EXPRESSION_FLOAT_CONSTANT,  
                            statementContents = (FloatConstantExpression value:_)}) ->

                  do putAsm
                       ["dq DATA_TYPE_FLOAT, " ++ show value ++ "\n"]

                (Statement {statementID = EXPRESSION_STRING_CONSTANT,  
                            statementContents = (StringConstantExpression value:_)}) ->

                  do let stringID = lookupString value strings
                     putAsm
                       ["dq DATA_TYPE_STRING, SC" ++ show stringID ++ "\n"]

compileRead :: Statement -> CodeTransformation ()
compileRead (Statement {statementContents = variableList}) =

        do mapM_ putRead variableList

putRead :: Statement -> CodeTransformation ()
putRead (Statement {statementContents = (statement:_)}) =
  
  do symbols <- gets compileStateSymbols
     localSymbols <- gets compileStateLocalSymbols
     nameSpace <- gets compileStateNameSpace

     case statement of
                
       (Statement {statementID = EXPRESSION_IDENTIFIER}) ->
         do putReadToVariable statement symbols localSymbols nameSpace

       (Statement {statementID = EXPRESSION_FIELD_ACCESS}) ->
         do putReadToField statement symbols localSymbols nameSpace

       (Statement {statementID = EXPRESSION_FUNCTION_CALL}) ->
         do putReadToArray (getFunctionCallOperands (statementContents statement)) symbols localSymbols nameSpace  

putReadToVariable :: Statement -> SymbolTable -> SymbolTable -> Symbol -> CodeTransformation ()
putReadToVariable statement symbols localSymbols nameSpace =
  do let sourceName = identifierExpressionValue (getInitialStatement statement)
         name = removeTypeTag (map toLower sourceName)
         dataType =
           case lookupVariable name symbols localSymbols nameSpace of
             (Variable {variableType = type_}) -> type_
             (LocalAutomaticVariable {localAutomaticVariableType = type_}) -> type_

     anticipateFatalError statement

     case dataType of

       VARIABLE_TYPE_INT ->

         do r1 <- reserveRegisterForFunctionCallWithPreference (functionCallRegisters !! 0)
            r2 <- reserveRegisterForFunctionCallWithPreference (functionCallRegisters !! 1)
            putAsm
              ["mov " ++ registerName r1 ++ ", " ++ decorateVariableName name symbols localSymbols nameSpace ++ "\n",
               "mov " ++ registerName r2 ++ ", DATA_TYPE_INT\n"]
            insertFunctionCall "read"
              [RawRegister (registerName r1) VARIABLE_TYPE_INT,
               RawRegister (registerName r2) VARIABLE_TYPE_INT]

       VARIABLE_TYPE_FLOAT ->

         do r1 <- reserveRegisterForFunctionCallWithPreference (functionCallRegisters !! 0)
            r2 <- reserveRegisterForFunctionCallWithPreference (functionCallRegisters !! 1)
            
            putAsm
              ["mov " ++ registerName r1 ++ ", " ++ decorateVariableName name symbols localSymbols nameSpace ++ "\n",
               "mov " ++ registerName r2 ++ ", DATA_TYPE_FLOAT\n"]
            insertFunctionCall "read"
              [RawRegister (registerName r1) VARIABLE_TYPE_INT,
               RawRegister (registerName r2) VARIABLE_TYPE_INT]

       VARIABLE_TYPE_STRING ->

         do r1 <- reserveRegisterForFunctionCallWithPreference (functionCallRegisters !! 0)
                  
            putAsm
              ["mov " ++ registerName r1 ++ ", [" ++ decorateVariableName name symbols localSymbols nameSpace ++ "]\n"]

            insertFunctionCall "free_string"
              [RawRegister (registerName r1) VARIABLE_TYPE_STRING]

            r1 <- reserveRegisterForFunctionCallWithPreference (functionCallRegisters !! 0)
            r2 <- reserveRegisterForFunctionCallWithPreference (functionCallRegisters !! 1)

            putAsm
              ["mov " ++ registerName r1 ++ ", " ++ decorateVariableName name symbols localSymbols nameSpace ++ "\n",
               "mov " ++ registerName r2 ++ ", DATA_TYPE_STRING\n"]
            insertFunctionCall "read"
              [RawRegister (registerName r1) VARIABLE_TYPE_INT,
               RawRegister (registerName r2) VARIABLE_TYPE_INT]

putReadToArray :: (Statement,[Statement]) -> SymbolTable -> SymbolTable -> Symbol -> CodeTransformation ()
putReadToArray (identifier,arguments) symbols localSymbols nameSpace =

  do let sourceName = identifierExpressionValue (getInitialStatement identifier)
         name = removeTypeTag (map toLower sourceName)
         (Array {arrayType = (VARIABLE_TYPE_ARRAY dataType)}) = lookupVariable name symbols localSymbols nameSpace
                     
     anticipateFatalError identifier
                     
     case dataType of

       VARIABLE_TYPE_INT ->

         do insertArrayAccess name arguments False
            register <- gets currentRegister
            r1 <- reserveRegisterForFunctionCallWithPreference (functionCallRegisters !! 0)
            r2 <- reserveRegisterForFunctionCallWithPreference (functionCallRegisters !! 1)
            putAsm
              ["mov " ++ registerName r1 ++ ", " ++ registerName register ++ "\n",
               "mov " ++ registerName r2 ++ ", DATA_TYPE_INT\n"]
            insertFunctionCall "read"
              [RawRegister (registerName r1) VARIABLE_TYPE_INT,
               RawRegister (registerName r2) VARIABLE_TYPE_INT]

       VARIABLE_TYPE_FLOAT ->

         do insertArrayAccess name arguments False
            register <- gets currentRegister
            r1 <- reserveRegisterForFunctionCallWithPreference (functionCallRegisters !! 0)
            r2 <- reserveRegisterForFunctionCallWithPreference (functionCallRegisters !! 1)
            putAsm
              ["mov " ++ registerName r1 ++ ", " ++ registerName register ++ "\n",
               "mov " ++ registerName r2 ++ ", DATA_TYPE_FLOAT\n"]
            insertFunctionCall "read"
              [RawRegister (registerName r1) VARIABLE_TYPE_INT,
               RawRegister (registerName r2) VARIABLE_TYPE_INT]

       VARIABLE_TYPE_STRING ->

         do insertArrayAccess name arguments False
            register <- gets currentRegister
            r1 <- reserveRegisterForFunctionCallWithPreference (functionCallRegisters !! 0)
            putAsm
              ["mov " ++ registerName r1 ++ ", [" ++ registerName register ++ "]\n"]

            insertFunctionCall "free_string"
              [RawRegister (registerName r1) VARIABLE_TYPE_STRING]

            r1 <- reserveRegisterForFunctionCallWithPreference (functionCallRegisters !! 0)
            r2 <- reserveRegisterForFunctionCallWithPreference (functionCallRegisters !! 1)

            putAsm
              ["mov " ++ registerName r1 ++ ", " ++ registerName register ++ "\n",
               "mov " ++ registerName r2 ++ ", DATA_TYPE_STRING\n"]
            insertFunctionCall "read"
              [RawRegister (registerName r1) VARIABLE_TYPE_INT,
               RawRegister (registerName r2) VARIABLE_TYPE_INT]

putReadToField :: Statement -> SymbolTable -> SymbolTable -> Symbol -> CodeTransformation ()
putReadToField expression symbols localSymbols nameSpace =

  do types <- gets compileStateTypes
     let dataType = getExpressionType expression symbols localSymbols types
    
     case dataType of

       VARIABLE_TYPE_INT ->

         do compileFieldAccessExpression False expression
            register <- gets currentRegister

            r1 <- reserveRegisterForFunctionCallWithPreference (functionCallRegisters !! 0)
            r2 <- reserveRegisterForFunctionCallWithPreference (functionCallRegisters !! 1)
            putAsm
              ["mov " ++ registerName r1 ++ ", " ++ registerName register ++ "\n",
               "mov " ++ registerName r2 ++ ", DATA_TYPE_INT\n"]
            insertFunctionCall "read"
              [RawRegister (registerName r1) VARIABLE_TYPE_INT,
               RawRegister (registerName r2) VARIABLE_TYPE_INT]

       VARIABLE_TYPE_FLOAT ->

         do compileFieldAccessExpression False expression
            register <- gets currentRegister
            r1 <- reserveRegisterForFunctionCallWithPreference (functionCallRegisters !! 0)
            r2 <- reserveRegisterForFunctionCallWithPreference (functionCallRegisters !! 1)
            putAsm
              ["mov " ++ registerName r1 ++ ", " ++ registerName register ++ "\n",
               "mov " ++ registerName r2 ++ ", DATA_TYPE_FLOAT\n"]
            insertFunctionCall "read"
              [RawRegister (registerName r1) VARIABLE_TYPE_INT,
               RawRegister (registerName r2) VARIABLE_TYPE_INT]

       VARIABLE_TYPE_STRING ->

         do compileFieldAccessExpression False expression
            register <- gets currentRegister

            r1 <- reserveRegisterForFunctionCallWithPreference (functionCallRegisters !! 0)
            putAsm
              ["mov " ++ registerName r1 ++ ", [" ++ registerName register ++ "]\n"]

            insertFunctionCall "free_string"
              [RawRegister (registerName r1) VARIABLE_TYPE_STRING]

            r1 <- reserveRegisterForFunctionCallWithPreference (functionCallRegisters !! 0)
            r2 <- reserveRegisterForFunctionCallWithPreference (functionCallRegisters !! 1)

            putAsm
              ["mov " ++ registerName r1 ++ ", " ++ registerName register ++ "\n",
               "mov " ++ registerName r2 ++ ", DATA_TYPE_STRING\n"]
            insertFunctionCall "read"
              [RawRegister (registerName r1) VARIABLE_TYPE_INT,
               RawRegister (registerName r2) VARIABLE_TYPE_INT]

compileRestore :: Statement -> CodeTransformation ()
compileRestore (Statement {statementContents = (expression:_)}) =

        do let sourceName = identifierExpressionValue (getInitialStatement expression)
               name = map toLower sourceName
           symbols <- gets compileStateSymbols
           localSymbols <- gets compileStateLocalSymbols
           nameSpace <- gets compileStateNameSpace
           r1 <- reserveRegisterForFunctionCallWithPreference (functionCallRegisters !! 0)
           putAsm
             ["mov " ++ registerName r1 ++ ", BASIC_DATA." ++ name ++ "\n"]
           insertFunctionCall "restore"
             [RawRegister (registerName r1) VARIABLE_TYPE_INT]

anticipateFatalError :: Statement -> CodeTransformation ()
anticipateFatalError statement =
  do debug <- gets (optionDebug . configOptions . compileStateConfig)
     sourceFileName <- gets (configSourceFileName . compileStateConfig)
     symbols <- gets compileStateSymbols
     localSymbols <- gets compileStateLocalSymbols
     if debug
     then do insertFunctionCall "anticipate_fatal_error"
               [createMinimalStatement EXPRESSION_STRING_CONSTANT [StringConstantExpression ("\"" ++ sourceFileName ++ "\"")],
                createMinimalStatement EXPRESSION_INT_CONSTANT [IntConstantExpression (statementLineNumber statement)],
                createMinimalStatement EXPRESSION_INT_CONSTANT [IntConstantExpression (statementOffset statement)]]
     else return ()

compileFor :: Statement -> CodeTransformation ()
compileFor (Statement {statementContents = statements}) =

        do symbols <- gets compileStateSymbols
           localSymbols <- gets compileStateLocalSymbols
           nameSpace <- gets compileStateNameSpace
           
           let (Statement {statementContents = (initialiserExpression:limitExpression:rest)}:code:_) = statements
               initialiser = getInitialStatement initialiserExpression
               limit = getInitialStatement limitExpression
               (leftOperand, _) = getBinaryOperands (statementContents initialiser)
               counterName = map toLower (removeTypeTag (getIdentifierValue leftOperand))
               variable = lookupVariable counterName symbols localSymbols nameSpace
               dataType = case variable of
                               (Variable {variableType = VARIABLE_TYPE_INT}) -> VARIABLE_TYPE_INT
                               (Variable {variableType = VARIABLE_TYPE_FLOAT}) -> VARIABLE_TYPE_FLOAT
                               (LocalAutomaticVariable {localAutomaticVariableType = VARIABLE_TYPE_INT}) -> VARIABLE_TYPE_INT
                               (LocalAutomaticVariable {localAutomaticVariableType = VARIABLE_TYPE_FLOAT}) -> VARIABLE_TYPE_FLOAT
                               _ -> VARIABLE_TYPE_INT
               step = getInitialStatement (head rest)
               positiveStep =
                 if rest /= []
                 then let reduced = reduceConstantExpression step symbols localSymbols in
                          case reduced of
                               (Statement {statementID = EXPRESSION_INT_CONSTANT}) ->
                                 getIntConstantValue reduced > 0
                               (Statement {statementID = EXPRESSION_FLOAT_CONSTANT}) ->
                                 getFloatConstantValue reduced > 0.0
                               (Statement {statementID = EXPRESSION_STRING_CONSTANT}) ->
                                 (read (removeQuotes (getStringConstantValue reduced)) :: Int) > 0
                 else True

           case dataType of

             VARIABLE_TYPE_INT ->

               do setDataType VARIABLE_TYPE_INT
                  compileExpression initialiser
                  resetRegisterState

                  nextLabelID
                  startLabelID <- gets compileStateLabelID

                  nextLabelID
                  escapeLabelID <- gets compileStateLabelID

                  addExitLabelID escapeLabelID

                  putAsm
                    [generateLabelName startLabelID ++ ":\n\n"]

                  setDataType VARIABLE_TYPE_INT
                  compileExpression leftOperand
                  counterRegister <- gets currentRegister

                  setDataType VARIABLE_TYPE_INT
                  compileExpression limit
                  limitRegister <- gets currentRegister
                  
                  putAsm
                    ["cmp " ++ registerName counterRegister ++ ", " ++ registerName limitRegister ++ "\n"]
                  if positiveStep
                  then putAsm
                         ["jg " ++ generateLabelName escapeLabelID ++ "\n\n"]
                  else putAsm
                         ["jl " ++ generateLabelName escapeLabelID ++ "\n\n"]
                  deallocateRegisters [counterRegister,limitRegister]
                  resetRegisterState
                  
                  compileCode code
                  
                  if rest /= []
                  then do setDataType VARIABLE_TYPE_INT
                          compileExpression leftOperand
                          counterRegister <- gets currentRegister
                          setDataType VARIABLE_TYPE_INT
                          compileExpression step
                          stepRegister <- gets currentRegister
                          putAsm
                            ["add " ++ registerName counterRegister ++ ", " ++ registerName stepRegister ++ "\n",
                             "mov [" ++ decorateVariableName counterName symbols localSymbols nameSpace ++ "], " ++ registerName counterRegister ++ "\n",
                             "jmp " ++ generateLabelName startLabelID ++ "\n\n"]
                          deallocateRegisters [counterRegister,stepRegister]
                  else do setDataType VARIABLE_TYPE_INT
                          compileExpression leftOperand
                          counterRegister <- gets currentRegister
                          
                          putAsm
                            ["inc " ++ registerName counterRegister ++ "\n",
                             "mov [" ++ decorateVariableName counterName symbols localSymbols nameSpace ++ "], " ++ registerName counterRegister ++ "\n",
                             "jmp " ++ generateLabelName startLabelID ++ "\n\n"]
                          deallocateRegisters [counterRegister]

                  resetRegisterState

                  putAsm
                    [generateLabelName escapeLabelID ++ ":\n\n"]

                  removeExitLabelID

             VARIABLE_TYPE_FLOAT ->
               do setDataType VARIABLE_TYPE_FLOAT
                  compileExpression initialiser
                  resetRegisterState

                  nextLabelID
                  startLabelID <- gets compileStateLabelID

                  nextLabelID
                  escapeLabelID <- gets compileStateLabelID

                  addExitLabelID escapeLabelID

                  putAsm
                    [generateLabelName startLabelID ++ ":\n\n"]

                  setDataType VARIABLE_TYPE_FLOAT
                  compileExpression leftOperand

                  allocateFloatRegister
                  setDataType VARIABLE_TYPE_FLOAT
                  compileExpression limit
                  putAsm
                    ["fcomip st0, st1\n",
                     "fstp st0\n",
                    "fwait\n"]
                  if positiveStep
                  then putAsm
                         ["jb " ++ generateLabelName escapeLabelID ++ "\n\n"]
                  else putAsm
                         ["ja " ++ generateLabelName escapeLabelID ++ "\n\n"]
                  deallocateFloatRegister
                  deallocateFloatRegister
                  resetRegisterState

                  compileCode code

                  allocateFloatRegister
                  allocateFloatRegister

                  if rest /= []
                  then do setDataType VARIABLE_TYPE_FLOAT
                          compileExpression leftOperand
                   
                          setDataType VARIABLE_TYPE_FLOAT
                          compileExpression step
                          putAsm
                            ["faddp st1, st0\n",
                             "fstp qword [" ++ decorateVariableName counterName symbols localSymbols nameSpace ++ "]\n",
                             "fwait\n",
                             "jmp " ++ generateLabelName startLabelID ++ "\n\n"]
                  else do setDataType VARIABLE_TYPE_FLOAT
                          compileExpression leftOperand
                          putAsm
                            ["fld1\n",
                             "faddp st1, st0\n",
                             "fstp qword [" ++ decorateVariableName counterName symbols localSymbols nameSpace ++ "]\n",
                             "fwait\n",
                             "jmp " ++ generateLabelName startLabelID ++ "\n\n"]
                     
                  deallocateFloatRegister
                  deallocateFloatRegister
                  resetRegisterState

                  putAsm
                    [generateLabelName escapeLabelID ++ ":\n\n"]

                  removeExitLabelID

compileForEach :: Statement -> CodeTransformation ()
compileForEach (Statement {statementContents = statements}) =

  do symbols <- gets compileStateSymbols
     localSymbols <- gets compileStateLocalSymbols
     nameSpace <- gets compileStateNameSpace
           
     let (Statement {statementContents = (initialiserExpression:_)}:code:_) = statements
         initialiser = getInitialStatement initialiserExpression
         (leftOperand,rightOperand) = getBinaryOperands (statementContents initialiser)
         iteratorName = map toLower (removeTypeTag (getIdentifierValue leftOperand))
         variable = lookupVariable iteratorName symbols localSymbols nameSpace
         dataType = variableType variable
         sourceTypeName = getIdentifierValue rightOperand
         typeName = map toLower sourceTypeName

     nextLabelID
     startLabelID <- gets compileStateLabelID

     nextLabelID
     skipLabelID <- gets compileStateLabelID

     nextLabelID
     escapeLabelID <- gets compileStateLabelID

     addExitLabelID escapeLabelID
     
     r1 <- reserveRegisterForFunctionCallWithPreference (functionCallRegisters !! 0)
     r2 <- reserveRegisterForFunctionCallWithPreference (functionCallRegisters !! 1)

     putAsm
       ["mov " ++ registerName r1 ++ ", [" ++ decorateLinkedListName typeName ++ "]\n",
        "lea " ++ registerName r2 ++ ", [" ++ decorateVariableName iteratorName symbols localSymbols nameSpace ++ "]\n"]

     insertFunctionCall "init_for_each"
       [RawRegister (registerName r1) VARIABLE_TYPE_INT,
        RawRegister (registerName r2) VARIABLE_TYPE_INT]

     result <- gets currentRegister
     putAsm
       ["cmp " ++ registerName result ++ ", 0\n",
        "jz " ++ generateLabelName skipLabelID ++ "\n"]

     putAsm
       [generateLabelName startLabelID ++ ":\n\n"]
     
     compileCode code
     
     r1 <- reserveRegisterForFunctionCallWithPreference (functionCallRegisters !! 0)

     putAsm
       ["mov " ++ registerName r1 ++ ", [" ++ decorateLinkedListName typeName ++ "]\n"]
       
     insertFunctionCall "next_for_each"
       [RawRegister (registerName r1) VARIABLE_TYPE_INT]

     result <- gets currentRegister
     
     putAsm
       ["cmp " ++ registerName result ++ ", 0\n",
        "jnz " ++ generateLabelName startLabelID ++ "\n\n"]

     resetRegisterState

     putAsm
       [generateLabelName escapeLabelID ++ ":\n\n"]

     r1 <- reserveRegisterForFunctionCallWithPreference (functionCallRegisters !! 0)

     putAsm
       ["mov " ++ registerName r1 ++ ", [" ++ decorateLinkedListName typeName ++ "]\n"]

     insertFunctionCall "final_for_each"
       [RawRegister (registerName r1) VARIABLE_TYPE_INT]

     putAsm
       [generateLabelName skipLabelID ++ ":\n\n"]

     removeExitLabelID

compileWhile :: Statement -> CodeTransformation ()
compileWhile (Statement {statementContents = (condition:code:_)}) =

  do symbols <- gets compileStateSymbols
     localSymbols <- gets compileStateLocalSymbols
     types <- gets compileStateTypes
     
     nextLabelID
     whileLabelID <- gets compileStateLabelID
     nextLabelID
     escapeLabelID <- gets compileStateLabelID

     addExitLabelID escapeLabelID

     putAsm
       [generateLabelName whileLabelID ++ ":\n\n"]

     setDataType VARIABLE_TYPE_INT
     compileExpression condition
     register <- gets currentRegister

     putAsm
       ["cmp " ++ registerName register ++ ", 0\n",
        "jz " ++ generateLabelName escapeLabelID ++ "\n"]
       
     compileCode code

     putAsm
       ["jmp " ++ generateLabelName whileLabelID ++ "\n\n"]
           
     putAsm
       [generateLabelName escapeLabelID ++ ":\n\n"]

     removeExitLabelID

compileRepeat :: Statement -> CodeTransformation ()
compileRepeat (Statement {statementContents = (code:footer:_)}) =

  do symbols <- gets compileStateSymbols
     localSymbols <- gets compileStateLocalSymbols
     types <- gets compileStateTypes
     let forever = statementID footer == STATEMENT_FOREVER
         condition =
           if forever
           then EmptyStatement
           else ((head . statementContents) footer)

     nextLabelID
     repeatLabelID <- gets compileStateLabelID
     nextLabelID
     escapeLabelID <- gets compileStateLabelID

     addExitLabelID escapeLabelID

     putAsm
       [generateLabelName repeatLabelID ++ ":\n\n"]

     compileCode code

     if forever
     then putAsm
            ["jmp " ++ generateLabelName repeatLabelID ++ "\n\n"]
     else do setDataType VARIABLE_TYPE_INT
             compileExpression condition
             register <- gets currentRegister
             putAsm
               ["cmp " ++ registerName register ++ ", 0\n",
                "jz " ++ generateLabelName repeatLabelID ++ "\n"]

     putAsm
       [generateLabelName escapeLabelID ++ ":\n\n"]

     removeExitLabelID

addExitLabelID :: Int -> CodeTransformation ()
addExitLabelID id =
  do state <- get
     put state {compileStateExitLabelIDs = (id:(compileStateExitLabelIDs state))}     

removeExitLabelID :: CodeTransformation ()
removeExitLabelID =
  do state <- get
     put state {compileStateExitLabelIDs = drop 1 (compileStateExitLabelIDs state)}
     
compileExit :: Statement -> CodeTransformation ()
compileExit statement =
  do exitLabelID <- gets (head . compileStateExitLabelIDs)
     putAsm
       ["jmp " ++ generateLabelName exitLabelID ++ "\n\n"]

compileIf :: Statement -> CodeTransformation ()
compileIf (Statement {statementContents = (Statement {statementContents = (expression:code:_)}:rest)}) =

        do nextLabelID
           escapeLabelID <- gets compileStateLabelID
           nextLabelID
           skipLabelID <- gets compileStateLabelID

           setDataType VARIABLE_TYPE_INT
           compileExpression expression
           register <- gets currentRegister

           putAsm
             ["cmp " ++ registerName register ++ ", 0\n",
              "jz " ++ generateLabelName skipLabelID ++ "\n\n"]

           compileCode code

           putAsm
             ["jmp " ++ generateLabelName escapeLabelID ++ "\n\n"]

           putAsm
             [generateLabelName skipLabelID ++ ":\n\n"]

           compileElseStatements escapeLabelID rest

           putAsm
             [generateLabelName escapeLabelID ++ ":\n\n"]

compileElseStatements :: Int -> [Statement] -> CodeTransformation ()
compileElseStatements escapeLabelID (statement:rest) =
  do case statementID statement of
          STATEMENT_ELSE_IF ->
            do let (expression:code:_) = statementContents statement

               nextLabelID
               skipLabelID <- gets compileStateLabelID

               setDataType VARIABLE_TYPE_INT
               compileExpression expression
               register <- gets currentRegister

               if rest == []
               then do putAsm
                        ["cmp " ++ registerName register ++ ", 0\n",
                         "jz " ++ generateLabelName escapeLabelID ++ "\n\n"]
                       compileCode code
                       putAsm
                         ["jmp " ++ generateLabelName escapeLabelID ++ "\n\n"]
               else do putAsm
                         ["cmp " ++ registerName register ++ ", 0\n",
                          "jz " ++ generateLabelName skipLabelID ++ "\n\n"]
                       compileCode code
                       putAsm
                         ["jmp " ++ generateLabelName escapeLabelID ++ "\n\n",
                          generateLabelName skipLabelID ++ ":\n\n"]
                       compileElseStatements escapeLabelID rest

          STATEMENT_ELSE ->
            do let (code:_) = statementContents statement              
               compileCode code

compileElseStatements _ _ =
  do return ()

restoreCPUContext :: CPUContext -> CodeTransformation ()
restoreCPUContext cpuContext =
  do state <- get
     put state {compileStateRegisters = cpuContext}
    
compileSelect :: Statement -> CodeTransformation ()
compileSelect (Statement {statementContents = (expression:rest)}) =

  do symbols <- gets compileStateSymbols
     localSymbols <- gets compileStateLocalSymbols
     types <- gets compileStateTypes
     
     nextLabelID
     escapeLabelID <- gets compileStateLabelID
     let dataType = getExpressionType expression symbols localSymbols types

     setDataType dataType
     compileExpression expression

     case dataType of
          VARIABLE_TYPE_INT ->
            do register <- gets currentRegister
               cpuContext <- gets compileStateRegisters
               compileIntCaseStatements cpuContext register escapeLabelID rest
          VARIABLE_TYPE_FLOAT ->
            do cpuContext <- gets compileStateRegisters
               compileFloatCaseStatements cpuContext escapeLabelID rest
          VARIABLE_TYPE_STRING ->
            do register <- gets currentRegister
               cpuContext <- gets compileStateRegisters
               compileStringCaseStatements cpuContext register escapeLabelID rest

     putAsm
       [generateLabelName escapeLabelID ++ ":\n\n"]

compileIntCaseStatements :: CPUContext -> Register -> Int -> [Statement] -> CodeTransformation ()
compileIntCaseStatements prevCPUContext selectRegister escapeLabelID (statement:rest) =
  do case statementID statement of
          STATEMENT_CASE ->
            do let (expression:code:_) = statementContents statement
               nextLabelID
               skipLabelID <- gets compileStateLabelID
               
               cpuContext <- gets compileStateRegisters
               restoreCPUContext prevCPUContext

               setDataType VARIABLE_TYPE_INT
               compileExpression expression
               caseRegister <- gets currentRegister

               if rest == []
               then do putAsm
                        ["cmp " ++ registerName selectRegister ++ ", " ++ registerName caseRegister ++ "\n",
                         "jnz " ++ generateLabelName escapeLabelID ++ "\n\n"]
                       compileCode code
                       putAsm
                         ["jmp " ++ generateLabelName escapeLabelID ++ "\n\n"]
               else do putAsm
                         ["cmp " ++ registerName selectRegister ++ ", " ++ registerName caseRegister ++ "\n",
                          "jnz " ++ generateLabelName skipLabelID ++ "\n\n"]
                       compileCode code
                       putAsm
                         ["jmp " ++ generateLabelName escapeLabelID ++ "\n\n",
                          generateLabelName skipLabelID ++ ":\n\n"]
                       compileIntCaseStatements prevCPUContext selectRegister escapeLabelID rest

          STATEMENT_DEFAULT ->
            do let (code:_) = statementContents statement              
               compileCode code

compileIntCaseStatements _ _ _ _ =
  do return ()

compileFloatCaseStatements :: CPUContext -> Int -> [Statement] -> CodeTransformation ()
compileFloatCaseStatements prevCPUContext escapeLabelID (statement:rest) =
  do case statementID statement of
          STATEMENT_CASE ->
            do let (expression:code:_) = statementContents statement
               nextLabelID
               skipLabelID <- gets compileStateLabelID
               
               cpuContext <- gets compileStateRegisters
               restoreCPUContext prevCPUContext

               setDataType VARIABLE_TYPE_FLOAT
               compileExpression expression

               if rest == []
               then do putAsm
                        ["fcomip\n",
                         "jnz " ++ generateLabelName escapeLabelID ++ "\n\n"]
                       compileCode code
                       putAsm
                         ["jmp " ++ generateLabelName escapeLabelID ++ "\n\n"]
               else do putAsm
                         ["fcomip\n",
                          "jnz " ++ generateLabelName skipLabelID ++ "\n\n"]
                       compileCode code
                       putAsm
                         ["jmp " ++ generateLabelName escapeLabelID ++ "\n\n",
                          generateLabelName skipLabelID ++ ":\n\n"]
                       compileFloatCaseStatements prevCPUContext escapeLabelID rest

          STATEMENT_DEFAULT ->
            do let (code:_) = statementContents statement              
               compileCode code

compileFloatCaseStatements _ _ _ =
  do return ()

#if LINUX==1 || MAC_OS==1

compileStringCaseStatements :: CPUContext -> Register -> Int -> [Statement] -> CodeTransformation ()
compileStringCaseStatements prevCPUContext selectRegister escapeLabelID (statement:rest) =
  do symbols <- gets compileStateSymbols
     localSymbols <- gets compileStateLocalSymbols
     nameSpace <- gets compileStateNameSpace
     types <- gets compileStateTypes
     case statementID statement of
          STATEMENT_CASE ->
            do let (expression:code:_) = statementContents statement
               nextLabelID
               skipLabelID <- gets compileStateLabelID
               
               cpuContext <- gets compileStateRegisters
               restoreCPUContext prevCPUContext

               setDataType VARIABLE_TYPE_STRING
               compileExpression expression
               caseRegister <- gets currentRegister

               if rest == []
               then do putStringComparison selectRegister caseRegister
                       if isLeakyExpression symbols localSymbols types nameSpace expression
                       then do putStringFree caseRegister
                       else return ()
                       putAsm
                         ["cmp eax, 0\n",
                          "jnz " ++ generateLabelName escapeLabelID ++ "\n\n"]
                       putStringFree selectRegister
                       compileCode code
                       putAsm
                         ["jmp " ++ generateLabelName escapeLabelID ++ "\n\n"]
               else do putStringComparison selectRegister caseRegister
                       if isLeakyExpression symbols localSymbols types nameSpace expression
                       then do putStringFree caseRegister
                       else return ()
                       putAsm
                         ["cmp eax, 0\n",
                          "jnz " ++ generateLabelName skipLabelID ++ "\n\n"]
                       putStringFree selectRegister
                       compileCode code
                       putAsm
                         ["jmp " ++ generateLabelName escapeLabelID ++ "\n\n",
                          generateLabelName skipLabelID ++ ":\n\n"]
                       compileStringCaseStatements prevCPUContext selectRegister escapeLabelID rest

          STATEMENT_DEFAULT ->
            do let (code:_) = statementContents statement              
               compileCode code
  where putStringComparison selectRegister caseRegister =
          do putAsm
               ["mov rdi, " ++ registerName selectRegister ++ "\n",
                "mov rsi, " ++ registerName caseRegister ++ "\n",
                "mov rax, 0\n",
                "call [" ++ bbFunctionPrefix ++ "compare_strings wrt ..got]\n"]
        putStringFree caseRegister =
          do putAsm
               ["push rax\n",
                "push rax\n",
                "mov rdi, " ++ registerName caseRegister ++ "\n",
                "mov rax, 0\n",
                "call [" ++ bbFunctionPrefix ++ "free_string wrt ..got]\n",
                "pop rax\n",
                "pop rax\n"]

compileStringCaseStatements _ _ _ _ =
  do return ()
     
#elif WINDOWS == 1

compileStringCaseStatements :: CPUContext -> Register -> Int -> [Statement] -> CodeTransformation ()
compileStringCaseStatements prevCPUContext selectRegister escapeLabelID (statement:rest) =
  do symbols <- gets compileStateSymbols
     localSymbols <- gets compileStateLocalSymbols
     nameSpace <- gets compileStateNameSpace
     types <- gets compileStateTypes
     case statementID statement of
          STATEMENT_CASE ->
            do let (expression:code:_) = statementContents statement
               nextLabelID
               skipLabelID <- gets compileStateLabelID
               
               cpuContext <- gets compileStateRegisters
               restoreCPUContext prevCPUContext

               setDataType VARIABLE_TYPE_STRING
               compileExpression expression
               caseRegister <- gets currentRegister

               if rest == []
               then do putStringComparison selectRegister caseRegister
                       if isLeakyExpression symbols localSymbols types nameSpace expression
                       then putStringFree caseRegister
                       else return ()
                       putAsm
                         ["cmp eax, 0\n",
                          "jnz " ++ generateLabelName escapeLabelID ++ "\n\n"]
                       compileCode code
                       putAsm
                         ["jmp " ++ generateLabelName escapeLabelID ++ "\n\n"]
               else do putStringComparison selectRegister caseRegister
                       if isLeakyExpression symbols localSymbols types nameSpace expression
                       then putStringFree caseRegister
                       else return ()
                       putAsm
                         ["cmp eax, 0\n",
                          "jnz " ++ generateLabelName skipLabelID ++ "\n\n"]
                       compileCode code
                       putAsm
                         ["jmp " ++ generateLabelName escapeLabelID ++ "\n\n",
                          generateLabelName skipLabelID ++ ":\n\n"]
                       compileStringCaseStatements prevCPUContext selectRegister escapeLabelID rest

          STATEMENT_DEFAULT ->
            do let (code:_) = statementContents statement              
               compileCode code
  where putStringComparison selectRegister caseRegister =
          do putAsm
               ["mov rcx, " ++ registerName selectRegister ++ "\n",
                "mov rdx, " ++ registerName caseRegister ++ "\n",
                "mov rax, 0\n",
                "sub rsp, 32\n",
                "call [" ++ bbFunctionPrefix ++ "compare_strings wrt ..got]\n",
                "add rsp, 32\n"]
        putStringFree caseRegister =
          do putAsm
               ["push rax\n",
                "push rax\n",
                "mov rcx, " ++ registerName caseRegister ++ "\n",
                "mov rax, 0\n",
                "sub rsp, 32\n",
                "call [" ++ bbFunctionPrefix ++ "free_string wrt ..got]\n",
                "add rsp, 32\n",
                "pop rax\n",
                "pop rax\n"]

compileStringCaseStatements _ _ _ _ =
  do return ()

#endif

compileLabel :: Statement -> CodeTransformation ()
compileLabel statement =

        do let (IdentifierExpression {identifierExpressionValue = sourceName}) = getInitialStatement statement
               name = map toLower sourceName
           putAsm
             [decorateLabelName (tail name) ++ ":\n\n"]

compileDataLabel :: Statement -> CodeTransformation ()
compileDataLabel statement =

        do let (IdentifierExpression {identifierExpressionValue = sourceName}) = getInitialStatement statement
               name = map toLower sourceName
           bucket <- getAsmBucket
           setAsmBucket data_
           putAsm
             [name ++ ":\n\n"]
           setAsmBucket bucket

compileOnGoto :: Statement -> CodeTransformation ()
compileOnGoto (Statement {statementContents = (predicate:destinations)}) =
        do let numDestinations = length destinations
               
           bucket <- getAsmBucket
           setAsmBucket globals_
           
           nextLabelID
           jumpTableLabelID <- gets compileStateLabelID
           
           putAsm
             ["\n",
              "JT" ++ (show jumpTableLabelID) ++ ":\n\n"]
             
           mapM_ putJumpTableElement destinations              
           
           setAsmBucket bucket
          
           setDataType VARIABLE_TYPE_INT
           compileExpression predicate
           register <- gets currentRegister
           
           nextLabelID
           errorLabelID <- gets compileStateLabelID
           nextLabelID
           skipLabelID <- gets compileStateLabelID
           
           putAsm
             ["cmp " ++ registerName register ++ ", 0\n",
              "jl " ++ generateLabelName errorLabelID ++ "\n",
              "cmp " ++ registerName register ++ ", " ++ show numDestinations ++ "\n",
              "jge " ++ generateLabelName errorLabelID ++ "\n",
              "imul " ++ registerName register ++ ", 8\n",
              "add " ++ registerName register ++ ", JT" ++ (show jumpTableLabelID) ++ "\n",
              "jmp " ++ " qword [" ++ registerName register ++ "]\n"]
           
           putAsm
             ["jmp " ++ generateLabelName skipLabelID ++ "\n"] 
           putAsm
             [generateLabelName errorLabelID ++ ":\n"]
             
           anticipateFatalError (head destinations)
             
           insertFunctionCall "fatal_error"
             [createMinimalStatement EXPRESSION_STRING_CONSTANT [StringConstantExpression runtimeErrorOnGotoRangeException]]

           putAsm
             [generateLabelName skipLabelID ++ ":\n"]


compileOnGosub :: Statement -> CodeTransformation ()
compileOnGosub (Statement {statementContents = (predicate:destinations)}) =
        do symbols <- gets compileStateSymbols
           localSymbols <- gets compileStateLocalSymbols
           nameSpace <- gets compileStateNameSpace
           
           let numDestinations = length destinations
               
           bucket <- getAsmBucket
           setAsmBucket globals_
           
           nextLabelID
           jumpTableLabelID <- gets compileStateLabelID
           
           putAsm
             ["\n",
              "JT" ++ (show jumpTableLabelID) ++ ":\n\n"]
             
           mapM_ putJumpTableElement destinations              
           
           setAsmBucket bucket
          
           setDataType VARIABLE_TYPE_INT
           compileExpression predicate
           register <- gets currentRegister
           
           nextLabelID
           errorLabelID <- gets compileStateLabelID
           nextLabelID
           returnLabelID <- gets compileStateLabelID
           nextLabelID
           skipLabelID <- gets compileStateLabelID
           
           putAsm
             ["cmp " ++ registerName register ++ ", 0\n",
              "jl " ++ generateLabelName errorLabelID ++ "\n",
              "cmp " ++ registerName register ++ ", " ++ show numDestinations ++ "\n",
              "jge " ++ generateLabelName errorLabelID ++ "\n",
              "imul " ++ registerName register ++ ", 8\n",
              "add " ++ registerName register ++ ", JT" ++ (show jumpTableLabelID) ++ "\n"]
             
           r1 <- reserveRegisterForFunctionCallWithPreference (functionCallRegisters !! 0)

           putAsm
             ["mov " ++ registerName r1 ++ ", " ++ generateLabelName returnLabelID ++ "\n"]

           insertFunctionCall "init_gosub"
             [RawRegister (registerName r1) VARIABLE_TYPE_INT]

           putAsm
             ["jmp qword [" ++ registerName register ++ "]\n"]

           putAsm
             [generateLabelName returnLabelID ++ ":\n"]

           putAsm
             ["jmp " ++ generateLabelName skipLabelID ++ "\n"] 
           putAsm
             [generateLabelName errorLabelID ++ ":\n"]
             
           anticipateFatalError (head destinations)
             
           insertFunctionCall "fatal_error"
             [createMinimalStatement EXPRESSION_STRING_CONSTANT [StringConstantExpression runtimeErrorOnGotoRangeException]]

           putAsm
             [generateLabelName skipLabelID ++ ":\n"]


putJumpTableElement :: Statement -> CodeTransformation ()
putJumpTableElement statement =
  
  do nameSpace <- gets compileStateNameSpace
     let identifierExpression = (getInitialStatement . getInitialStatement) statement
     if nameSpace == NO_NAMESPACE
     then putAsm
            ["dq " ++ osFunctionPrefix ++ "main" ++ decorateLabelName (map toLower (identifierExpressionValue identifierExpression)) ++ "\n"]
     else do let name = functionName nameSpace
             putAsm
               ["dq " ++ name ++ decorateLabelName (map toLower (identifierExpressionValue identifierExpression)) ++ "\n"]

addRuntimeErrorString :: String -> CodeTransformation ()
addRuntimeErrorString value =
        do state <- get
           let strings = compileStateStrings state
               size = Map.size strings
           put state {compileStateStrings = (Map.insert value (size + 1) strings)}

compileGoto :: Statement -> CodeTransformation ()
compileGoto statement =

        do let (Statement {statementContents = (destination:_)}) = getInitialStatement statement
               sourceName = identifierExpressionValue destination
               name = map toLower sourceName
           putAsm
             ["jmp " ++ decorateLabelName name ++ "\n\n"]

compileGosub :: Statement -> CodeTransformation ()
compileGosub statement =

  do symbols <- gets compileStateSymbols
     localSymbols <- gets compileStateLocalSymbols
     nameSpace <- gets compileStateNameSpace
     
     let (Statement {statementContents = (destination:_)}) = getInitialStatement statement
         sourceName = identifierExpressionValue destination
         name = map toLower sourceName

     anticipateFatalError statement
     
     nextLabelID
     returnLabelID <- gets compileStateLabelID

     r1 <- reserveRegisterForFunctionCallWithPreference (functionCallRegisters !! 0)

     putAsm
       ["mov " ++ registerName r1 ++ ", " ++ generateLabelName returnLabelID ++ "\n"]
     insertFunctionCall "init_gosub"
       [RawRegister (registerName r1) VARIABLE_TYPE_INT]
     
     putAsm
       ["jmp " ++ decorateLabelName name ++ "\n\n"]

     putAsm
       [generateLabelName returnLabelID ++ ":\n\n"]

compileReturn :: Statement -> CodeTransformation ()
compileReturn statement =
  do anticipateFatalError statement
     insertFunctionCall "final_gosub" []
     register <- gets currentRegister
     putAsm
       ["jmp " ++ registerName register ++ "\n\n"]

setCompileStateNameSpace :: Symbol -> CodeTransformation ()
setCompileStateNameSpace symbol =
  do state <- get
     put state {compileStateNameSpace = symbol}

#if LINUX==1 || MAC_OS==1
compileMultiFunction :: Statement -> CodeTransformation ()
compileMultiFunction  (Statement {statementContents = statements}) =

        do globalSymbols <- gets compileStateSymbols
           bucket <- getAsmBucket

           setAsmBucket functions_

           let (Statement {statementContents = (identifier:_)}:_:definition:_) = statements
               sourceName = identifierExpressionValue (getInitialStatement identifier)
               name = removeTypeTag (map toLower sourceName)
               function = lookupVariable name globalSymbols Map.empty NO_NAMESPACE
               parameters = functionParameters function
               localSymbols = functionSymbols function
               emptyStatements = take (length parameters) (repeat EmptyStatement)
               registerArguments = reverse $ extractRegisterArguments (zip parameters emptyStatements) 0 0 0
               gprArguments = filter (\a -> parameterType (fst a) /= VARIABLE_TYPE_FLOAT) registerArguments
               mmrArguments = filter (\a -> parameterType (fst a) == VARIABLE_TYPE_FLOAT) registerArguments
               stackArguments = reverse $ extractStackArguments (zip parameters emptyStatements) 0 0 0
               offset = 1
               localStrings = filter (\v -> localAutomaticVariableType v == VARIABLE_TYPE_STRING) ((snd . unzip) (Map.toList localSymbols))
               freeLocalString symbol =
                 do putAsm
                      ["mov rax, 0\n",
                       "mov rdi, [rbp + " ++ show (localAutomaticVariableAddress symbol) ++ "]\n",
                       "call [" ++ bbFunctionPrefix ++ "free_string wrt ..got]\n"]

               insertArgumentSpill ((parameter, _) : rest) index intIndex floatIndex =
                    
                 do let symbol = fromJust (Map.lookup (map toLower (removeTypeTag (parameterName parameter))) localSymbols)
                    case parameterType parameter of
                         VARIABLE_TYPE_INT ->

                           do putAsm
                                ["mov qword [rbp + " ++ show (index * (-8))  ++ "], " ++ registerName (functionCallRegisters !! intIndex) ++ "\n"]
                              insertArgumentSpill rest (index + 1) (intIndex + 1) floatIndex

                         VARIABLE_TYPE_FLOAT ->

                           do putAsm
                                ["movq [rbp + " ++ show (index * (-8)) ++ "], " ++ (multimediaRegisterNames !! floatIndex) ++ "\n"]
                              insertArgumentSpill rest (index + 1) intIndex (floatIndex + 1)

                         VARIABLE_TYPE_STRING ->

                           do putAsm
                                ["mov qword [rbp + " ++ show (index * (-8))  ++ "], " ++ registerName (functionCallRegisters !! intIndex) ++ "\n"]
                              insertArgumentSpill rest (index + 1) (intIndex + 1) floatIndex
                     
                         VARIABLE_TYPE_CUSTOM {} ->

                           do putAsm
                                ["mov qword [rbp + " ++ show (index * (-8))  ++ "], " ++ registerName (functionCallRegisters !! intIndex) ++ "\n"]
                              insertArgumentSpill rest (index + 1) (intIndex + 1) floatIndex
                              
               insertArgumentSpill _ index intIndex floatIndex =
                 do return ()

               insertStringDuplicate ((parameter, _) : rest) =
                    
                 do let symbol = fromJust (Map.lookup (map toLower (removeTypeTag (parameterName parameter))) localSymbols)
                    case parameterType parameter of
                         
                         VARIABLE_TYPE_STRING ->

                           do putAsm
                                ["mov rax, 0\n",
                                 "mov rdi, [rbp + " ++ show (localAutomaticVariableAddress symbol) ++ "]\n",
                                 "sub rsp, 8\n",
                                 "call [" ++ bbFunctionPrefix ++ "duplicate_string wrt ..got]\n",
                                 "add rsp, 8\n",
                                 "mov [rbp + " ++ show (localAutomaticVariableAddress symbol) ++ "], rax\n"]

                              insertStringDuplicate rest

                         _ ->

                           do insertStringDuplicate rest
                              
               insertStringDuplicate _ =

                           do return ()

           originalNameSpace <- gets compileStateNameSpace
           setCompileStateNameSpace function
           
           putAsm
             [(decorateUserFunctionName (removeTypeTag name)) ++ ":\n\n"]

           putAsm
             ["push rbp\n",
              "mov rbp, rsp\n",
              "push rbx\n",
              "push r12\n",
              "push r13\n",
              "push r14\n",
              "push r15\n"]
                         
           if even (functionNumLocals function)
           then putAsm
                  ["sub rsp, " ++ show (8 * (functionNumLocals function)) ++ "\n"]
           else putAsm
                  ["sub rsp, " ++ show (8 * (1 + functionNumLocals function)) ++ "\n"]
 
           insertArgumentSpill (reverse registerArguments) (numPreservedRegisters) 0 0
           insertStringDuplicate (registerArguments ++ stackArguments)

           if (functionNumLocals function - functionNumRegisterArguments function) > 0
           then {-- This code is more efficient but triggers a crash in the valgrind memory error checker... --}
                {-- putAsm
                  ["lea rdi, [rbp - " ++ show (8 * (length registerArguments + numPreservedRegisters)) ++ "]\n",
                   "mov rcx, " ++ show (functionNumLocals function - functionNumRegisterArguments function) ++ "\n",
                   "xor rax, rax\n",
                   "std\n",
                   "repnz stosq\n",
                   "cld\n"] --}
                
                putAsm
                  ["lea rdi, [rbp - " ++ show (8 * (length registerArguments + numPreservedRegisters)) ++ "]\n",
                   "mov rcx, " ++ show (functionNumLocals function - functionNumRegisterArguments function) ++ "\n",
                   "xor rax, rax\n",
                   ".localZeroStack:\n",
                   "cmp rcx, 0\n",
                   "jz .localZeroStackDone\n",
                   "mov [rdi], rax\n",
                   "dec rcx\n",
                   "sub rdi, 8\n",
                   "jmp .localZeroStack\n",
                   ".localZeroStackDone:\n"]
                   --}

           else return ()

           localSymbols <- getLocalSymbols
           setLocalSymbols (functionSymbols function)

           compileCode definition

           case functionType function of

                VARIABLE_TYPE_INT ->

                  do putAsm
                       ["mov rax, 0\n"]

                VARIABLE_TYPE_FLOAT ->

                  do putAsm
                       ["movq xmm0, rax\n"]

                VARIABLE_TYPE_STRING ->
                  do putAsm
                       ["mov rax, NULL_STRING\n"]

                VARIABLE_TYPE_CUSTOM {} ->

                  do putAsm
                       ["mov rax, 0\n"]

           setLocalSymbols localSymbols

           putAsm
             ["call .localCleanup\n"]           

           if even (functionNumLocals function)
           then putAsm
                  ["add rsp, " ++ show (8 * (functionNumLocals function)) ++ "\n"]
           else putAsm
                  ["add rsp, " ++ show (8 * (1 + functionNumLocals function)) ++ "\n"]

           putAsm
             ["pop r15\n",
              "pop r14\n",
              "pop r13\n",
              "pop r12\n",
              "pop rbx\n",
              "pop rbp\n"]

           putAsm
              ["ret\n\n"]

           putAsm
             [".localCleanup:\n\n"]

           mapM_ freeLocalString localStrings
             
           putAsm
             ["ret\n\n"]

           setAsmBucket bucket
           setCompileStateNameSpace originalNameSpace

compileMultiFunctionReturn :: Statement -> CodeTransformation ()
compileMultiFunctionReturn Statement {statementContents = expression:_} =

  do globalSymbols <- gets compileStateSymbols
     localSymbols <- gets compileStateLocalSymbols
     function <- gets compileStateNameSpace

     setDataType (functionType function)
     compileExpression expression
     case functionType function of
       VARIABLE_TYPE_INT ->

         do register <- gets currentRegister
            putAsm
              ["mov rax, " ++ registerName register ++ "\n"]

            putAsm
              ["push rax\npush rax\n",
               "call .localCleanup\n",
               "pop rax\npop rax\n"]

       VARIABLE_TYPE_FLOAT ->

         do putAsm
              ["sub rsp, 8\n",
               "fstp qword [rsp]\n",
               "fwait\n",
               "mov rax, [rsp]\n",
               "push rax\n",
               "call .localCleanup\n",
               "pop rax\n",
               "movq xmm0, rax\n",
               "add rsp, 8\n"]

       VARIABLE_TYPE_STRING ->
                    
         do if expressionIsAtomic expression
            then do register <- gets currentRegister
                    r1 <- reserveRegisterForFunctionCallWithPreference (functionCallRegisters !! 0)
                    putAsm
                      ["mov " ++ registerName r1 ++  ", " ++ registerName register ++ "\n"]
                    insertFunctionCall "duplicate_string"
                      [RawRegister (registerName r1) VARIABLE_TYPE_STRING]
                             
            else do register <- gets currentRegister
                    putAsm
                      ["mov rax, " ++ registerName register ++ "\n"]
            putAsm
              ["push rax\npush rax\n",
               "call .localCleanup\n",
               "pop rax\npop rax\n"]

       VARIABLE_TYPE_CUSTOM {} ->

         do register <- gets currentRegister
            putAsm
              ["mov rax, " ++ registerName register ++ "\n"]

     if even (functionNumLocals function)
     then putAsm
            ["add rsp, " ++ show (8 * (functionNumLocals function)) ++ "\n"]
     else putAsm
            ["add rsp, " ++ show (8 * (1 + functionNumLocals function)) ++ "\n"]
                  
     putAsm
       ["pop r15\n",
        "pop r14\n",
        "pop r13\n",
        "pop r12\n",
        "pop rbx\n",
        "pop rbp\n"]

     putAsm
       ["ret\n\n"]

compileMultiFunctionReturn _ =
  do function <- gets compileStateNameSpace

     case functionType function of

       VARIABLE_TYPE_INT ->

         do putAsm
              ["mov rax, 0\n"]

       VARIABLE_TYPE_FLOAT ->

         do putAsm
              ["mov rax, 0\n",
               "movq xmm0, " ++ registerName rax ++ "\n"]

       VARIABLE_TYPE_STRING ->
         do putAsm
              ["mov rax, NULL_STRING\n"]

       VARIABLE_TYPE_CUSTOM {} ->

         do putAsm
              ["mov rax, 0\n"]

     if even (functionNumLocals function)
     then putAsm
            ["add rsp, " ++ show (8 * (functionNumLocals function)) ++ "\n"]
     else putAsm
            ["add rsp, " ++ show (8 * (1 + functionNumLocals function)) ++ "\n"]
                  
     putAsm
       ["pop r15\n",
        "pop r14\n",
        "pop r13\n",
        "pop r12\n",
        "pop rbx\n",
        "pop rbp\n"]

     putAsm
       ["ret\n\n"]

#elif WINDOWS==1

compileMultiFunction :: Statement -> CodeTransformation ()
compileMultiFunction  (Statement {statementContents = statements}) =

        do globalSymbols <- gets compileStateSymbols
           bucket <- getAsmBucket

           setAsmBucket functions_

           let (Statement {statementContents = (identifier:_)}:_:definition:_) = statements
               sourceName = identifierExpressionValue (getInitialStatement identifier)
               name = removeTypeTag (map toLower sourceName)
               function = lookupVariable name globalSymbols Map.empty NO_NAMESPACE
               parameters = functionParameters function
               localSymbols = functionSymbols function
               emptyStatements = take (length parameters) (repeat EmptyStatement)
               registerArguments = reverse $ extractRegisterArguments (zip parameters emptyStatements)
               gprArguments = filter (\a -> parameterType (fst a) /= VARIABLE_TYPE_FLOAT) registerArguments
               mmrArguments = filter (\a -> parameterType (fst a) == VARIABLE_TYPE_FLOAT) registerArguments
               stackArguments = reverse $ extractStackArguments (zip parameters emptyStatements)
               offset = 1
               localStrings = filter (\v -> localAutomaticVariableType v == VARIABLE_TYPE_STRING) ((snd . unzip) (Map.toList localSymbols))
               freeLocalString symbol =
                 do putAsm
                      ["mov rax, 0\n",
                       "mov rcx, [rbp + " ++ show (localAutomaticVariableAddress symbol) ++ "]\n",
                       "sub rsp, 32\n",
                       "call [" ++ bbFunctionPrefix ++ "free_string wrt ..got]\n",
                       "add rsp, 32\n"]
                      
               insertArgumentSpill ((parameter, _) : rest) index intIndex floatIndex =
                    
                 do let symbol = fromJust (Map.lookup (map toLower (removeTypeTag (parameterName parameter))) localSymbols)
                    case parameterType parameter of
                         VARIABLE_TYPE_INT ->

                           do putAsm
                                ["mov qword [rbp + " ++ show (index * (-8))  ++ "], " ++ registerName (functionCallRegisters !! intIndex) ++ "\n"]
                              insertArgumentSpill rest (index + 1) (intIndex + 1) (floatIndex + 1)

                         VARIABLE_TYPE_FLOAT ->

                           do putAsm
                                ["movq [rbp + " ++ show (index * (-8)) ++ "], " ++ (take 4 multimediaRegisterNames !! floatIndex) ++ "\n"]
                              insertArgumentSpill rest (index + 1) (intIndex + 1) (floatIndex + 1)

                         VARIABLE_TYPE_STRING ->

                           do putAsm
                                ["mov qword [rbp + " ++ show (index * (-8)) ++ "], " ++ registerName (functionCallRegisters !! intIndex) ++ "\n"]
                              insertArgumentSpill rest (index + 1) (intIndex + 1) (floatIndex + 1)

                         VARIABLE_TYPE_CUSTOM {} ->

                           do putAsm
                                ["mov qword [rbp + " ++ show (index * (-8))  ++ "], " ++ registerName (functionCallRegisters !! intIndex) ++ "\n"]
                              insertArgumentSpill rest (index + 1) (intIndex + 1) (floatIndex + 1)
                              
               insertArgumentSpill _ index intIndex floatIndex =
                 do return ()

               insertStringDuplicate ((parameter, _) : rest) =
                    
                 do let symbol = fromJust (Map.lookup (map toLower (removeTypeTag (parameterName parameter))) localSymbols)
                    case parameterType parameter of
                         
                         VARIABLE_TYPE_STRING ->

                           do putAsm
                                ["mov rax, 0\n",
                                 "mov rcx, [rbp + " ++ show (localAutomaticVariableAddress symbol) ++ "]\n",
                                 "sub rsp, 40\n",
                                 "call [" ++ bbFunctionPrefix ++ "duplicate_string wrt ..got]\n",
                                 "add rsp, 40\n",
                                 "mov [rbp + " ++ show (localAutomaticVariableAddress symbol) ++ "], rax\n"]

                              insertStringDuplicate rest

                         _ ->

                           do insertStringDuplicate rest
                              
               insertStringDuplicate _ =

                           do return ()

           originalNameSpace <- gets compileStateNameSpace
           setCompileStateNameSpace function
           
           putAsm
             [(decorateUserFunctionName (removeTypeTag name)) ++ ":\n\n"]
             
           putAsm
             ["push rbp\n",
              "mov rbp, rsp\n",
              "push rbx\n",
              "push rsi\n",
              "push rdi\n",
              "push r12\n",
              "push r13\n",
              "push r14\n",
              "push r15\n"]
                         
           if even (functionNumLocals function)
           then putAsm
                  ["sub rsp, " ++ show (8 * (functionNumLocals function)) ++ "\n"]
           else putAsm
                  ["sub rsp, " ++ show (8 * (1 + functionNumLocals function)) ++ "\n"]
 
           insertArgumentSpill (reverse registerArguments) (numPreservedRegisters) 0 0
           insertStringDuplicate (registerArguments ++ stackArguments)

           if (functionNumLocals function - functionNumRegisterArguments function) > 0
           then {-- This code is more efficient but triggers a crash in the valgrind memory error checker... 
                 putAsm
                  ["lea rdi, [rbp - " ++ show (8 * (length registerArguments + numPreservedRegisters)) ++ "]\n",
                   "mov rcx, " ++ show (functionNumLocals function - functionNumRegisterArguments function) ++ "\n",
                   "xor rax, rax\n",
                   "std\n",
                   "repnz stosq\n",
                   "cld\n"]
                --}
                putAsm
                  ["lea rdi, [rbp - " ++ show (8 * (length registerArguments + numPreservedRegisters)) ++ "]\n",
                   "mov rcx, " ++ show (functionNumLocals function - functionNumRegisterArguments function) ++ "\n",
                   "xor rax, rax\n",
                   ".localZeroStack:\n",
                   "cmp rcx, 0\n",
                   "jz .localZeroStackDone\n",
                   "mov [rdi], rax\n",
                   "dec rcx\n",
                   "sub rdi, 8\n",
                   "jmp .localZeroStack\n",
                   ".localZeroStackDone:\n"]

           else return ()

           localSymbols <- getLocalSymbols
           setLocalSymbols (functionSymbols function)

           compileCode definition

           case functionType function of

                VARIABLE_TYPE_INT ->

                  do putAsm
                       ["mov rax, 0\n"]

                VARIABLE_TYPE_FLOAT ->

                  do putAsm
                       ["movq xmm0, rax\n"]

                VARIABLE_TYPE_STRING ->
                  do putAsm
                       ["mov rax, NULL_STRING\n"]

                VARIABLE_TYPE_CUSTOM {} ->

                  do putAsm
                       ["mov rax, 0\n"]

           setLocalSymbols localSymbols

           putAsm
             ["call .localCleanup\n"]           

           if even (functionNumLocals function)
           then putAsm
                  ["add rsp, " ++ show (8 * (functionNumLocals function)) ++ "\n"]
           else putAsm
                  ["add rsp, " ++ show (8 * (1 + functionNumLocals function)) ++ "\n"]

           putAsm
             ["pop r15\n",
              "pop r14\n",
              "pop r13\n",
              "pop r12\n",
              "pop rdi\n",
              "pop rsi\n",
              "pop rbx\n",
              "pop rbp\n"]

           putAsm
              ["ret\n\n"]

           putAsm
             [".localCleanup:\n\n"]

           mapM_ freeLocalString localStrings
             
           putAsm
             ["ret\n\n"]

           setAsmBucket bucket
           setCompileStateNameSpace originalNameSpace

compileMultiFunctionReturn :: Statement -> CodeTransformation ()
compileMultiFunctionReturn Statement {statementContents = expression:_} =

  do globalSymbols <- gets compileStateSymbols
     localSymbols <- gets compileStateLocalSymbols
     function <- gets compileStateNameSpace

     setDataType (functionType function)
     compileExpression expression
     case functionType function of
       VARIABLE_TYPE_INT ->

         do register <- gets currentRegister
            putAsm
              ["mov rax, " ++ registerName register ++ "\n"]

            putAsm
              ["push rax\npush rax\n",
               "call .localCleanup\n",
               "pop rax\npop rax\n"]

       VARIABLE_TYPE_FLOAT ->

         do putAsm
              ["sub rsp, 8\n",
               "fstp qword [rsp]\n",
               "fwait\n",
               "mov rax, [rsp]\n",
               "push rax\n",
               "call .localCleanup\n",
               "pop rax\n",
               "movq xmm0, rax\n",
               "add rsp, 8\n"]

       VARIABLE_TYPE_STRING ->
                    
         do if expressionIsAtomic expression
            then do register <- gets currentRegister
                    r1 <- reserveRegisterForFunctionCallWithPreference (functionCallRegisters !! 0)
                    putAsm
                      ["mov " ++ registerName r1 ++  ", " ++ registerName register ++ "\n"]
                    insertFunctionCall "duplicate_string"
                      [RawRegister (registerName r1) VARIABLE_TYPE_STRING]
                             
            else do register <- gets currentRegister
                    putAsm
                      ["mov rax, " ++ registerName register ++ "\n"]
            putAsm
              ["push rax\npush rax\n",
               "call .localCleanup\n",
               "pop rax\npop rax\n"]

       VARIABLE_TYPE_CUSTOM {} ->

         do register <- gets currentRegister
            putAsm
              ["mov rax, " ++ registerName register ++ "\n"]

     if even (functionNumLocals function)
     then putAsm
            ["add rsp, " ++ show (8 * (functionNumLocals function)) ++ "\n"]
     else putAsm
            ["add rsp, " ++ show (8 * (1 + functionNumLocals function)) ++ "\n"]
                  
     putAsm
       ["pop r15\n",
        "pop r14\n",
        "pop r13\n",
        "pop r12\n",
        "pop rdi\n",
        "pop rsi\n",
        "pop rbx\n",
        "pop rbp\n"]

     putAsm
       ["ret\n\n"]

compileMultiFunctionReturn _ =
  do function <- gets compileStateNameSpace

     case functionType function of

       VARIABLE_TYPE_INT ->

         do putAsm
              ["mov rax, 0\n"]

       VARIABLE_TYPE_FLOAT ->

         do putAsm
              ["mov rax, 0\n",
               "movq xmm0, " ++ registerName rax ++ "\n"]

       VARIABLE_TYPE_STRING ->
         do putAsm
              ["mov rax, NULL_STRING\n"]

       VARIABLE_TYPE_CUSTOM {} ->

         do putAsm
              ["mov rax, 0\n"]

     if even (functionNumLocals function)
     then putAsm
            ["add rsp, " ++ show (8 * (functionNumLocals function)) ++ "\n"]
     else putAsm
            ["add rsp, " ++ show (8 * (1 + functionNumLocals function)) ++ "\n"]
                  
     putAsm
       ["pop r15\n",
        "pop r14\n",
        "pop r13\n",
        "pop r12\n",
        "pop rdi\n",
        "pop rsi\n",
        "pop rbx\n",
        "pop rbp\n"]

     putAsm
       ["ret\n\n"]


#endif

compileConst :: Statement -> CodeTransformation ()
compileConst _ =
  do return ()

compileGlobal :: Statement -> CodeTransformation ()
compileGlobal  Statement {statementContents = expressionList} =
  do mapM_ compileGlobalVariableDeclaration expressionList
     
compileGlobalVariableDeclaration :: Statement -> CodeTransformation ()
compileGlobalVariableDeclaration Statement {statementContents = (expression:_)} =
  do if statementID expression == EXPRESSION_ASSIGN
     then compileExpression expression
     else return ()

compileLocal :: Statement -> CodeTransformation ()
compileLocal  Statement {statementContents = expressionList} =
  do mapM_ compileLocalAutomaticVariableDeclaration expressionList
     
compileLocalAutomaticVariableDeclaration :: Statement -> CodeTransformation ()
compileLocalAutomaticVariableDeclaration Statement {statementContents = (expression:_)} =
  do if statementID expression == EXPRESSION_ASSIGN
     then compileExpression expression
     else return ()

#if LINUX==1 || MAC_OS==1
putEndFunction :: CodeTransformation ()
putEndFunction =
  do putAsm
       ["bbu_end:\n\n",
        "push rdi\n",
        "call " ++ osFunctionPrefix ++ "main.cleanup\n",
        "mov rax, 0\n",
        "mov rdi, [rsp]\n",
        "call [" ++ osFunctionPrefix ++ "exit wrt ..got]\n"]
#elif WINDOWS==1
putEndFunction :: CodeTransformation ()
putEndFunction =
  do putAsm
       ["bbu_end:\n\n",
        "push rcx\n",
        "call " ++ osFunctionPrefix ++ "main.cleanup\n",
        "mov rax, 0\n",
        "mov rcx, [rsp]\n",
        "call [" ++ osFunctionPrefix ++ "exit wrt ..got]\n"]
#endif

#if LINUX==1 || MAC_OS==1
compileEnd :: Statement -> CodeTransformation ()
compileEnd (Statement {statementContents = (exitCode:_)}) =
  do console <- gets (optionConsole . configOptions . compileStateConfig)
     let finalFunctionName =
           if console
           then "final_libkoshka_core"
           else "final_libkoshka_mm"

     insertFunctionCall finalFunctionName []
     putAsm
       ["sub rsp, 8\n",
        "mov rdi, " ++ show (intConstantExpressionValue (getInitialStatement exitCode)) ++ "\n",
        "call bbu_end\n"]
   
compileEnd _ =
  do console <- gets (optionConsole . configOptions . compileStateConfig)
     let finalFunctionName =
           if console
           then "final_libkoshka_core"
           else "final_libkoshka_mm"
     insertFunctionCall finalFunctionName []
     putAsm
       ["sub rsp, 8\n",
        "mov rdi, 0\n",
        "call bbu_end\n"]
        
#elif WINDOWS==1
compileEnd :: Statement -> CodeTransformation ()
compileEnd (Statement {statementContents = (exitCode:g_)}) =
  do console <- gets (optionConsole . configOptions . compileStateConfig)
     let finalFunctionName =
           if console
           then "final_libkoshka_core"
           else "final_libkoshka_mm"
     insertFunctionCall finalFunctionName []
     putAsm
       ["sub rsp, 40\n",
        "mov rcx, " ++ show (intConstantExpressionValue (getInitialStatement exitCode)) ++ "\n",
        "call bbu_end\n"]
        
compileEnd _ =
  do console <- gets (optionConsole . configOptions . compileStateConfig)
     let finalFunctionName =
           if console
           then "final_libkoshka_core"
           else "final_libkoshka_mm"
     insertFunctionCall finalFunctionName []
     putAsm
       ["sub rsp, 40\n",
        "mov rcx, 0\n",
        "call bbu_end\n"]
        
#endif

compileReducedExpression :: Statement -> CodeTransformation ()

compileReducedExpression (Statement {statementID = STATEMENT_EXPRESSION,
                                     statementContents = (expression:_)}) =
  compileReducedExpression expression

compileReducedExpression (Statement {statementID = EXPRESSION_ASSIGN, 
                                     statementContents = contents}) =

  do symbols <- gets compileStateSymbols
     localSymbols <- gets compileStateLocalSymbols
     nameSpace <- gets compileStateNameSpace
     types <- gets compileStateTypes

     let (leftOperand, rightOperand) = getBinaryOperands contents

     case leftOperand of
             
       (Statement {statementID = EXPRESSION_FUNCTION_CALL}) ->
                 
         do let (identifierExpression, arguments) = getFunctionCallOperands (statementContents leftOperand)
                identifier = getInitialStatement identifierExpression
                sourceName = identifierExpressionValue identifier
                name = removeTypeTag (map toLower sourceName)
                Array _ (VARIABLE_TYPE_ARRAY arrayType) numDimensions = lookupVariable name symbols localSymbols nameSpace

            case arrayType of

              VARIABLE_TYPE_INT ->

                do setDataType VARIABLE_TYPE_INT
                   compileExpression rightOperand
                   rightRegister <- gets currentRegister
                   insertArrayAccess name arguments False
                   leftRegister <- gets currentRegister
                   putAsm ["mov qword [" ++ registerName leftRegister ++ "], " ++ registerName rightRegister ++ "\n\n"]

              VARIABLE_TYPE_FLOAT ->

                do setDataType VARIABLE_TYPE_FLOAT
                   compileExpression rightOperand
                   insertArrayAccess name arguments False
                   leftRegister <- gets currentRegister
                   putAsm ["fstp qword [" ++ registerName leftRegister ++ "]\n"]
                   deallocateFloatRegister

              VARIABLE_TYPE_STRING ->

                do setDataType VARIABLE_TYPE_STRING
                   compileExpression rightOperand
                   rightRegister <- gets currentRegister
                   setDataType VARIABLE_TYPE_INT
                   insertArrayAccess name arguments False
                   leftRegister <- gets currentRegister

                   if isLeakyExpression symbols localSymbols types nameSpace rightOperand || getExpressionType rightOperand symbols localSymbols types /= VARIABLE_TYPE_STRING
                   then do putAsm ["mov qword [" ++ registerName leftRegister ++ "], " ++ registerName rightRegister ++ "\n\n"]
                   else do r1 <- reserveRegisterForFunctionCallWithPreference (functionCallRegisters !! 0)
                           r2 <- reserveRegisterForFunctionCallWithPreference (functionCallRegisters !! 1)
                           putAsm
                             ["mov " ++ registerName r1 ++ ", " ++ registerName leftRegister ++ "\n",
                              "mov " ++ registerName r2 ++ ", " ++ registerName rightRegister ++ "\n"]

                           insertFunctionCall "copy_string"
                             [RawRegister (registerName r1) VARIABLE_TYPE_INT,
                              RawRegister (registerName r2) VARIABLE_TYPE_STRING]

              VARIABLE_TYPE_CUSTOM {} ->

                do setDataType VARIABLE_TYPE_INT
                   compileExpression rightOperand
                   source <- gets currentRegister
                   setDataType VARIABLE_TYPE_INT
                   insertArrayAccess name arguments False
                   dest <- gets currentRegister
            
                   putAsm
                     ["mov [" ++ registerName dest ++ "], " ++ registerName source ++ "\n"]

       (Statement {statementID = EXPRESSION_FIELD_ACCESS}) ->

         do let type_ = getExpressionType leftOperand symbols localSymbols types

            case type_ of

              VARIABLE_TYPE_INT ->

                do setDataType VARIABLE_TYPE_INT
                   compileExpression rightOperand
                   source <- gets currentRegister
                   setDataType VARIABLE_TYPE_INT
                   compileFieldAccessExpression False leftOperand
                   dest <- gets currentRegister
            
                   putAsm
                     ["mov [" ++ registerName dest ++ "], " ++ registerName source ++ "\n"]

              VARIABLE_TYPE_FLOAT ->

                do setDataType VARIABLE_TYPE_FLOAT
                   compileExpression rightOperand
                   compileFieldAccessExpression False leftOperand
                   leftRegister <- gets currentRegister
                   putAsm ["fstp qword [" ++ registerName leftRegister ++ "]\n"]
                   deallocateFloatRegister

              VARIABLE_TYPE_STRING ->

                do setDataType VARIABLE_TYPE_STRING
                   compileExpression rightOperand
                   rightRegister <- gets currentRegister
                   setDataType VARIABLE_TYPE_INT
                   compileFieldAccessExpression False leftOperand
                   leftRegister <- gets currentRegister

                   if isLeakyExpression symbols localSymbols types nameSpace rightOperand || getExpressionType rightOperand symbols localSymbols types /= VARIABLE_TYPE_STRING
                   then do insertFunctionCall "free_string"
                             [leftOperand]
                           putAsm ["mov qword [" ++ registerName leftRegister ++ "], " ++ registerName rightRegister ++ "\n\n"]
                   else do r1 <- reserveRegisterForFunctionCallWithPreference (functionCallRegisters !! 0)
                           r2 <- reserveRegisterForFunctionCallWithPreference (functionCallRegisters !! 1)
                           putAsm
                             ["mov " ++ registerName r1 ++ ", " ++ registerName leftRegister ++ "\n",
                              "mov " ++ registerName r2 ++ ", " ++ registerName rightRegister ++ "\n"]

                           insertFunctionCall "copy_string"
                             [RawRegister (registerName r1) VARIABLE_TYPE_INT,
                              RawRegister (registerName r2) VARIABLE_TYPE_STRING]

              VARIABLE_TYPE_CUSTOM {} ->

                do setDataType VARIABLE_TYPE_INT
                   compileExpression rightOperand
                   source <- gets currentRegister
                   setDataType VARIABLE_TYPE_INT
                   compileFieldAccessExpression False leftOperand
                   dest <- gets currentRegister
            
                   putAsm
                     ["mov [" ++ registerName dest ++ "], " ++ registerName source ++ ";fack\n"]

       (Statement {statementID = EXPRESSION_IDENTIFIER}) ->

         do let name = removeTypeTag ( map toLower (getIdentifierValue leftOperand))
                symbol = lookupVariable name symbols localSymbols nameSpace
                destType = case symbol of
                             (Variable {variableType = type_}) ->
                               type_
                             (LocalAutomaticVariable {localAutomaticVariableType = type_}) ->
                               type_

            case destType of

              VARIABLE_TYPE_INT ->

                do setDataType VARIABLE_TYPE_INT
                   compileExpression rightOperand
                   register <- gets currentRegister
                   putAsm ["mov qword [" ++ decorateVariableName name symbols localSymbols nameSpace ++ "], " ++ registerName register ++ "\n\n"]

              VARIABLE_TYPE_FLOAT ->

                do setDataType VARIABLE_TYPE_FLOAT
                   compileExpression rightOperand
                   putAsm ["fstp qword [" ++ decorateVariableName name symbols localSymbols nameSpace ++ "]\n",
                           "fwait\n\n"]
                   deallocateFloatRegister

              VARIABLE_TYPE_STRING ->

                do setDataType VARIABLE_TYPE_STRING
                   compileExpression rightOperand
                   register <- gets currentRegister

                   if isLeakyExpression symbols localSymbols types nameSpace rightOperand || getExpressionType rightOperand symbols localSymbols types /= VARIABLE_TYPE_STRING
                   then do insertFunctionCall "free_string"
                             [leftOperand]
                           putAsm ["mov qword [" ++ decorateVariableName name symbols localSymbols nameSpace ++ "], " ++ registerName register ++ "\n\n"]
                   else do r1 <- reserveRegisterForFunctionCallWithPreference (functionCallRegisters !! 0)
                           r2 <- reserveRegisterForFunctionCallWithPreference (functionCallRegisters !! 1)
                           putAsm
                             ["lea " ++ registerName r1 ++ ", [" ++ decorateVariableName name symbols localSymbols nameSpace ++ "]\n",
                              "mov " ++ registerName r2 ++ ", " ++ registerName register ++ "\n"]

                           insertFunctionCall "copy_string"
                             [RawRegister (registerName r1) VARIABLE_TYPE_INT,
                              RawRegister (registerName r2) VARIABLE_TYPE_STRING]

              VARIABLE_TYPE_CUSTOM {customTypeName = sourceName} ->
                          
                do setDataType (VARIABLE_TYPE_CUSTOM name)
                   compileExpression rightOperand
                   register <- gets currentRegister
                   putAsm ["mov qword [" ++ decorateVariableName name symbols localSymbols nameSpace ++ "], " ++ registerName register ++ "\n\n"]

compileReducedExpression (Statement {statementID = EXPRESSION_GROUP,
                                     statementContents = (group:_)}) =
        compileExpression group

compileReducedExpression (Statement {statementID = EXPRESSION_FUNCTION_CALL, 
                                     statementContents = (operand:arguments)}) =

        do dataType <- gets getDataType
           symbols <- gets compileStateSymbols
           
           let (IdentifierExpression sourceName) = getInitialStatement operand
               name = removeTypeTag (map toLower sourceName)
               symbol = lookupVariable name symbols Map.empty NO_NAMESPACE

           case symbol of

                (Function {functionType = returnType, functionOrigin = origin,functionParameters = parameters}) ->

                  do insertFunctionCall name (arguments ++ populateDefaultValues parameters)

                     case returnType of

                          VARIABLE_TYPE_INT ->

                            do return ()

                          VARIABLE_TYPE_FLOAT ->

                            do allocateFloatRegister
                               register <- gets currentRegister
                               putAsm
                                 ["push " ++ registerName register ++ "\n",
                                  "fld qword [rsp]\n",
                                  "pop rax\n"]
                               deallocateRegisters [register]

                          VARIABLE_TYPE_STRING ->

                            do return ()

                          VARIABLE_TYPE_CUSTOM {} ->

                            do return ()

                     setDataType dataType
                     insertTypeFilter returnType

                (Array name arrayType numDimensions) ->

                  do insertArrayAccess name arguments True

compileReducedExpression (Statement {statementID = EXPRESSION_IDENTIFIER, 
                                     statementContents = (identifier:_)}) =

        do state <- get
           symbols <- gets compileStateSymbols
           localSymbols <- gets compileStateLocalSymbols
           nameSpace <- gets compileStateNameSpace

           let name = removeTypeTag (map toLower (identifierExpressionValue identifier))
               variable = lookupVariable name symbols localSymbols nameSpace
           case variable of

             (Variable {variableType = VARIABLE_TYPE_INT}) ->

               do destType <- gets getDataType
                  case destType of

                       (VARIABLE_TYPE_INT) ->

                         do allocateRegister
                            register <- gets currentRegister
                            putAsm ["mov " ++ registerName register ++ ", [" ++ decorateVariableName name symbols localSymbols nameSpace ++ "]\n"]

                       (VARIABLE_TYPE_FLOAT) ->

                         do allocateFloatRegister
                            putAsm ["fild qword [" ++ decorateVariableName name symbols localSymbols nameSpace ++ "]\n"]

                       (VARIABLE_TYPE_STRING) ->

                         do allocateRegister
                            register <- gets currentRegister
                            putAsm ["mov " ++ registerName register ++ ", [" ++ decorateVariableName name symbols localSymbols nameSpace ++ "]\n"]
                            insertTypeFilter VARIABLE_TYPE_INT
                            
             (Variable {variableType = VARIABLE_TYPE_FLOAT}) ->

               do allocateFloatRegister
                  putAsm
                    ["fld qword [" ++ decorateVariableName name symbols localSymbols nameSpace ++ "]\n"]
                  insertTypeFilter VARIABLE_TYPE_FLOAT

             (Variable {variableType = VARIABLE_TYPE_STRING}) ->

               do allocateRegister
                  register <- gets currentRegister
                  putAsm
                    ["mov " ++ registerName register ++ ", [" ++ decorateVariableName name symbols localSymbols nameSpace ++ "]\n"]
                  insertTypeFilter VARIABLE_TYPE_STRING

             (Variable {variableType = VARIABLE_TYPE_CUSTOM {customTypeName = typeName}}) ->

               do allocateRegister
                  register <- gets currentRegister
                  putAsm
                    ["mov " ++ registerName register ++ ", [" ++ decorateVariableName name symbols localSymbols nameSpace ++ "]\n"]
                  insertTypeFilter (VARIABLE_TYPE_CUSTOM typeName)
                  
             (Array _ _ _) ->

               do allocateRegister
                  register <- gets currentRegister
                  putAsm ["mov " ++ registerName register ++ ", [" ++ decorateVariableName name symbols localSymbols nameSpace ++ "]\n"]
                  insertTypeFilter VARIABLE_TYPE_INT

             (LocalAutomaticVariable _ VARIABLE_TYPE_INT _ address) ->

               do allocateRegister
                  register <- gets currentRegister
                  putAsm ["mov " ++ registerName register ++ ", [rbp + " ++ show address ++ "]\n"]
                  insertTypeFilter VARIABLE_TYPE_INT

             (LocalAutomaticVariable _ VARIABLE_TYPE_FLOAT _ address) ->

               do allocateFloatRegister
                  putAsm
                    ["fld qword [rbp + " ++ show address ++ "]\n"]
                  insertTypeFilter VARIABLE_TYPE_FLOAT

             (LocalAutomaticVariable _ VARIABLE_TYPE_STRING _ address) ->

               do allocateRegister
                  register <- gets currentRegister
                  putAsm
                    ["mov " ++ registerName register ++ ", [rbp + " ++ show address ++ "]\n"]
                  insertTypeFilter VARIABLE_TYPE_STRING

             (LocalAutomaticVariable {localAutomaticVariableType = VARIABLE_TYPE_CUSTOM {customTypeName = typeName},
                                      localAutomaticVariableAddress = address}) ->

               do allocateRegister
                  register <- gets currentRegister
                  putAsm ["mov " ++ registerName register ++ ", [rbp + " ++ show address ++ "]\n"]
                  insertTypeFilter (VARIABLE_TYPE_CUSTOM typeName)
                  
             k -> error ("Critical error in function compileReducedExpression (EXPRESSION_IDENTIFIER).\n ")

compileReducedExpression (Statement {statementID = EXPRESSION_INT_CONSTANT,
                                     statementContents = (intConstant:_)}) =

        do state <- get
           symbols <- gets compileStateSymbols

           case getDataType state of

                VARIABLE_TYPE_INT ->

                  do allocateRegister
                     register <- gets currentRegister
                     putAsm ["mov " ++ registerName register ++ ", " ++ show (intConstantExpressionValue intConstant) ++ "\n"]

                VARIABLE_TYPE_FLOAT ->

                  do intTable <- gets compileStateInts
                     let int = lookupInt (intConstantExpressionValue intConstant) intTable
                     allocateFloatRegister
                     putAsm
                       ["fild qword [IC" ++ show int ++ "]\n"] 

                VARIABLE_TYPE_STRING ->

                  do insertFunctionCall "convert_int_to_string"
                       [createMinimalStatement EXPRESSION_INT_CONSTANT [IntConstantExpression (intConstantExpressionValue intConstant)]]
                     
                k -> error ("Critical error in function compileReducedExpression (EXPRESSION_INT_CONSTANT).\n ")

compileReducedExpression (Statement {statementID = EXPRESSION_FLOAT_CONSTANT,
                                     statementContents = (floatConstant:_)}) =

        do state <- get
           symbols <- gets compileStateSymbols
           localSymbols <- gets compileStateLocalSymbols
           nameSpace <- gets compileStateNameSpace

           case getDataType state of

                VARIABLE_TYPE_INT ->

                  do floats <- gets compileStateFloats
                     let id = lookupFloat (floatConstantExpressionValue floatConstant) floats
                     allocateRegister
                     register <- gets currentRegister
                     allocateFloatRegister
                     putAsm
                       ["sub rsp, 8\n",
                        "fld qword [FC" ++ show id ++ "]\n",
                        "fistp qword [rsp]\n",
                        "fwait\n",
                        "pop " ++ registerName register ++ "\n"]
                     deallocateFloatRegister

                VARIABLE_TYPE_FLOAT ->

                  do floats <- gets compileStateFloats
                     let id = lookupFloat (floatConstantExpressionValue floatConstant) floats
                     allocateFloatRegister
                     putAsm
                       ["fld qword [FC" ++ show id ++ "]\n"]

                VARIABLE_TYPE_STRING ->

                  do insertFunctionCall "convert_float_to_string"
                       [createMinimalStatement EXPRESSION_FLOAT_CONSTANT [FloatConstantExpression (floatConstantExpressionValue floatConstant)]]

compileReducedExpression (Statement {statementID = EXPRESSION_STRING_CONSTANT,
                                     statementContents = (stringConstant:_)}) =

        do state <- get
           symbols <- gets compileStateSymbols
           localSymbols <- gets compileStateLocalSymbols
           strings <- gets compileStateStrings

           let dataType = getDataType state
               stringID = lookupString (stringConstantExpressionValue stringConstant) strings

           case dataType of

                VARIABLE_TYPE_INT ->

                  do insertFunctionCall "convert_string_to_int"
                       [createMinimalStatement EXPRESSION_STRING_CONSTANT [StringConstantExpression (stringConstantExpressionValue stringConstant)]]
                     result <- gets currentRegister
                     allocateRegister
                     register <- gets currentRegister
                     putAsm ["mov " ++ registerName register ++ ", " ++ registerName result ++ "\n"]

                VARIABLE_TYPE_FLOAT ->

                  do insertFunctionCall "convert_string_to_float"
                       [createMinimalStatement EXPRESSION_STRING_CONSTANT [StringConstantExpression (stringConstantExpressionValue stringConstant)]]
                     register <- gets currentRegister
                     
                     allocateFloatRegister
                     putAsm
                       ["push " ++ registerName register ++ "\n",
                        "fld qword [rsp]\n",
                        "pop rax\n"]
                     deallocateRegisters [register]

                VARIABLE_TYPE_STRING ->

                  do allocateRegister
                     register <- gets currentRegister
                     putAsm ["mov " ++ registerName register ++ ", SC" ++ show stringID ++ "\n"]


compileReducedExpression (Statement {statementID = EXPRESSION_NULL}) =
  do allocateRegister
     register <- gets currentRegister
     putAsm
       ["mov " ++ registerName register ++ ", 0\n"]

compileReducedExpression expression =
  
        do symbols <- gets compileStateSymbols
           localSymbols <- gets compileStateLocalSymbols
           types <- gets compileStateTypes
           let expressionType = getExpressionType expression symbols localSymbols types
           chooseFunc expressionType symbols localSymbols types expression

        where chooseFunc expressionType symbols localSymbols types
                | isNewExpressionID expressionID =
                  compileNewExpression
                | isFieldAccessExpressionID expressionID =
                  compileFieldAccessExpression True
                | isStringExpression expression symbols localSymbols types =
                  compileStringExpression
                | isArithmeticExpressionID expressionID =
                  case expressionType of
                       VARIABLE_TYPE_INT -> compileIntArithmeticExpression
                       VARIABLE_TYPE_FLOAT -> compileFloatArithmeticExpression
                | isShiftExpressionID expressionID =
                  compileShiftExpression
                | isBitwiseExpressionID expressionID =
                  compileBitwiseExpression
                | isLogicalExpressionID expressionID =
                  compileLogicalExpression
                | isRelationalExpressionID expressionID =
                  case getRelationalExpressionOperandType expression symbols localSymbols types of
                    VARIABLE_TYPE_INT -> compileIntRelationalExpression
                    VARIABLE_TYPE_FLOAT -> compileFloatRelationalExpression
                    VARIABLE_TYPE_STRING -> compileStringRelationalExpression
                    VARIABLE_TYPE_CUSTOM {} -> compileCustomRelationalExpression
                | isTypeConversionExpressionID expressionID =
                  compileTypeConversionExpression
                | isTypeListExpressionID expressionID =
                  compileTypeListExpression
                | otherwise = error ("Unrecognised expression type.\n" ++ show (getExpressionType expression symbols localSymbols types))
              expressionID = statementID expression

compileFloatArithmeticExpression :: Statement -> CodeTransformation ()
compileFloatArithmeticExpression (Statement {statementID = expressionID,
                                        statementContents = contents}) =

        do dataType <- gets getDataType
           symbols <- gets compileStateSymbols
           localSymbols <- gets compileStateLocalSymbols

           case expressionID of

                EXPRESSION_NEG ->

                  do let operand = getUnaryOperand contents
                    
                     setDataType VARIABLE_TYPE_FLOAT
                     compileExpression operand

                     putAsm
                       ["fchs\n"]
                     
                EXPRESSION_POS ->
                  
                  do let operand = getUnaryOperand contents

                     setDataType VARIABLE_TYPE_FLOAT
                     compileExpression operand

                     putAsm
                       ["fabs\n"]

                EXPRESSION_POW ->

                  do let (leftOperand, rightOperand) = getBinaryOperands contents

                     insertFunctionCall "pow"
                       [leftOperand,
                        rightOperand]
                     register <- gets currentRegister
                     allocateFloatRegister
                     putAsm
                       ["push " ++ registerName register ++ "\n",
                        "fld qword [rsp]\n",
                        "pop rax\n"]
                     deallocateRegisters [register]

                EXPRESSION_MOD ->

                  do let (leftOperand, rightOperand) = getBinaryOperands contents

                     insertFunctionCall "fmod"
                       [leftOperand,
                        rightOperand]
                     register <- gets currentRegister
                     allocateFloatRegister
                     putAsm
                       ["push " ++ registerName register ++ "\n",
                        "fld qword [rsp]\n",
                        "pop rax\n"]
                     deallocateRegisters [register]            
                _ ->

                  do let (leftOperand, rightOperand) = getBinaryOperands contents

                     setDataType VARIABLE_TYPE_FLOAT
                     compileExpression leftOperand

                     setDataType VARIABLE_TYPE_FLOAT
                     compileExpression rightOperand

                     putAsm
                       [instruction ++ "\n"]

                     deallocateFloatRegister

           setDataType dataType
           insertTypeFilter VARIABLE_TYPE_FLOAT

        where instruction = case expressionID of
                                 EXPRESSION_ADD -> "faddp"
                                 EXPRESSION_SUB -> "fsubp"
                                 EXPRESSION_MUL -> "fmulp"
                                 EXPRESSION_DIV -> "fdivp"

compileIntArithmeticExpression :: Statement -> CodeTransformation ()
compileIntArithmeticExpression expression =

        do dataType <- gets getDataType
           symbols <- gets compileStateSymbols
           localSymbols <- gets compileStateLocalSymbols
           debug <- gets (optionDebug . configOptions . compileStateConfig)
     
           let contents = statementContents expression
               expressionID = statementID expression

           case expressionID of

                EXPRESSION_POW ->

                  do let (leftOperand, rightOperand) = getBinaryOperands contents

                     insertFunctionCall "pow"
                       [leftOperand,
                        rightOperand]
                     register <- gets currentRegister
                     putAsm
                       ["push " ++ registerName register ++ "\n",
                        "fld qword [rsp]\n",
                        "fistp qword [rsp]\n",
                        "fwait\n",
                        "pop " ++ registerName register ++ "\n"]

                EXPRESSION_DIV ->

                  do let (leftOperand, rightOperand) = getBinaryOperands contents

                     excludeSpecialRegisters expression
                     setDataType VARIABLE_TYPE_INT
                     compileExpression leftOperand
                     leftRegister <- gets currentRegister

                     excludeSpecialRegisters expression
                     setDataType VARIABLE_TYPE_INT
                     compileExpression rightOperand
                     rightRegister <- gets currentRegister
                     includeAllRegisters

                     anticipateFatalError rightOperand
                     if debug
                     then do nextLabelID
                             skipLabelID <- gets compileStateLabelID
                             putAsm
                               ["cmp " ++ registerName rightRegister ++ ", 0\n",
                                "jne " ++ generateLabelName skipLabelID ++ "\n"]
                             insertFunctionCall "fatal_error"
                               [createMinimalStatement EXPRESSION_STRING_CONSTANT [StringConstantExpression runtimeErrorDivisionByZeroException]]
                            
                             putAsm
                               [generateLabelName skipLabelID ++ ":\n\n"]
                     else return ()
   
                     putAsm
                       ["mov rax, " ++ registerName leftRegister ++ "\n",
                        "cqo\n",
                        "idiv " ++ registerName rightRegister ++ "\n",
                        "mov " ++ registerName leftRegister ++ ", rax\n"]

                     deallocateRegisters [rightRegister]
                     setCurrentRegister leftRegister

                EXPRESSION_MOD ->

                  do let (leftOperand, rightOperand) = getBinaryOperands contents

                     excludeSpecialRegisters expression
                     setDataType VARIABLE_TYPE_INT
                     compileExpression leftOperand
                     leftRegister <- gets currentRegister

                     excludeSpecialRegisters expression
                     setDataType VARIABLE_TYPE_INT
                     compileExpression rightOperand
                     rightRegister <- gets currentRegister
                     includeAllRegisters

                     anticipateFatalError rightOperand
                     if debug
                     then do nextLabelID
                             skipLabelID <- gets compileStateLabelID
                             putAsm
                               ["cmp " ++ registerName rightRegister ++ ", 0\n",
                                "jne " ++ generateLabelName skipLabelID ++ "\n"]
                             insertFunctionCall "fatal_error"
                               [createMinimalStatement EXPRESSION_STRING_CONSTANT [StringConstantExpression runtimeErrorDivisionByZeroException]]
                            
                             putAsm
                               [generateLabelName skipLabelID ++ ":\n\n"]
                     else return ()

                     putAsm
                       ["mov rax, " ++ registerName leftRegister ++ "\n",
                        "cqo\n",
                        "idiv " ++ registerName rightRegister ++ "\n",
                        "mov " ++ registerName leftRegister ++ ", rdx\n"]

                     deallocateRegisters [rightRegister]
                     setCurrentRegister leftRegister

                EXPRESSION_NEG ->

                  do let operand = getUnaryOperand contents

                     setDataType VARIABLE_TYPE_INT
                     compileExpression operand
                     register <- gets currentRegister

                     putAsm
                       ["neg " ++ registerName register ++ "\n"]
                     
                EXPRESSION_POS ->

                  do let operand = getUnaryOperand contents

                     setDataType VARIABLE_TYPE_INT
                     compileExpression operand
                     register <- gets currentRegister

                     putAsm
                       ["push " ++ registerName register ++ "\n",
                        "fild qword [rsp]\n",
                        "fabs\n",
                        "fistp qword [rsp]\n",
                        "fwait\n",
                        "pop " ++ registerName register ++ "\n"]

                _ ->

                  do let (leftOperand, rightOperand) = getBinaryOperands contents

                     setDataType VARIABLE_TYPE_INT
                     compileExpression leftOperand
                     leftRegister <- gets currentRegister

                     setDataType VARIABLE_TYPE_INT
                     compileExpression rightOperand
                     rightRegister <- gets currentRegister

                     putAsm
                       [instruction expressionID ++ " "  ++ registerName leftRegister ++ ", " ++ registerName rightRegister ++ "\n"]

                     deallocateRegisters [rightRegister]
                     setCurrentRegister leftRegister

                  
           setDataType dataType
           insertTypeFilter VARIABLE_TYPE_INT

        where instruction expressionID =
                case expressionID of
                  EXPRESSION_ADD -> "add"
                  EXPRESSION_SUB -> "sub"
                  EXPRESSION_MUL -> "imul"

compileBitwiseExpression :: Statement -> CodeTransformation ()
compileBitwiseExpression (Statement {statementID = expressionID,
                                     statementContents = contents}) =

        do dataType <- gets getDataType

           if expressionID == EXPRESSION_BITWISE_COMPLEMENT
           then do let operand = getUnaryOperand contents

                   setDataType VARIABLE_TYPE_INT
                   compileExpression operand

                   register <- gets currentRegister

                   putAsm
                     [instruction ++ " " ++ registerName register ++ "\n"]

           else do let (leftOperand, rightOperand) = getBinaryOperands contents

                   setDataType VARIABLE_TYPE_INT
                   compileExpression leftOperand

                   leftRegister <- gets currentRegister

                   setDataType VARIABLE_TYPE_INT
                   compileExpression rightOperand
                   
                   rightRegister <- gets currentRegister

                   putAsm
                     [instruction ++ " " ++ registerName leftRegister ++ ", " ++ registerName rightRegister ++ "\n"]

                   deallocateRegisters [rightRegister]
                   setCurrentRegister leftRegister

           setDataType dataType
           insertTypeFilter VARIABLE_TYPE_INT

        where instruction = case expressionID of
                                 EXPRESSION_AND -> "and"
                                 EXPRESSION_OR -> "or"
                                 EXPRESSION_XOR -> "xor"
                                 EXPRESSION_BITWISE_COMPLEMENT -> "not"

compileLogicalExpression :: Statement -> CodeTransformation ()
compileLogicalExpression (Statement {statementID = expressionID,
                                     statementContents = contents}) =

  do dataType <- gets getDataType
     let operand = getUnaryOperand contents

     setDataType VARIABLE_TYPE_INT
     compileExpression operand

     register <- gets currentRegister

     nextLabelID
     falseLabelID <- gets compileStateLabelID

     nextLabelID
     skipLabelID <- gets compileStateLabelID

     putAsm
       ["cmp " ++ registerName register ++ ", 0\n",
        "jz " ++ generateLabelName falseLabelID ++ "\n",
        "mov " ++ registerName register ++ ", 0\n",
        "jmp " ++ generateLabelName skipLabelID ++ "\n",
        generateLabelName falseLabelID ++ ":\n",
        "mov " ++ registerName register ++ ", 1\n",
        generateLabelName skipLabelID ++ ":\n"]

expressionContainsOperation :: Statement -> StatementID -> VariableType -> SymbolTable -> SymbolTable -> SymbolTable -> Bool
expressionContainsOperation expression operationID operationType symbols localSymbols types =

  let unaryOperand = getUnaryOperand (statementContents expression)
      (leftOperand,rightOperand) = getBinaryOperands (statementContents expression)
      expressionType = getExpressionType expression symbols localSymbols types
      expressionID = statementID expression in
  if expressionIsAtomic expression
  then False
  else if expressionIsUnary expression
       then if statementID expression == operationID && expressionType == operationType
            then True
            else expressionContainsOperation unaryOperand operationID operationType symbols localSymbols types
       else if expressionIsBinary expression
            then if statementID expression == operationID && expressionType == operationType
                 then True
                 else expressionContainsOperation leftOperand operationID operationType symbols localSymbols types || expressionContainsOperation rightOperand operationID operationType symbols localSymbols types
            else False

excludeSpecialRegisters :: Statement -> CodeTransformation ()
excludeSpecialRegisters expression =
  do symbols <- gets compileStateSymbols
     localSymbols <- gets compileStateLocalSymbols
     types <- gets compileStateTypes
     if expressionContainsOperation expression EXPRESSION_DIV VARIABLE_TYPE_INT symbols localSymbols types
     then excludeRegister rdx
     else return ()
     if expressionContainsOperation expression EXPRESSION_SHL VARIABLE_TYPE_INT symbols localSymbols types
     then excludeRegister rcx
     else return ()
     if expressionContainsOperation expression EXPRESSION_SHR VARIABLE_TYPE_INT symbols localSymbols types
     then excludeRegister rcx
     else return ()
     if expressionContainsOperation expression EXPRESSION_SAR VARIABLE_TYPE_INT symbols localSymbols types
     then excludeRegister rcx
     else return ()

compileShiftExpression :: Statement -> CodeTransformation ()
compileShiftExpression expression =

  do dataType <- gets getDataType

     let contents = statementContents expression
         expressionID = statementID expression
         (leftOperand, rightOperand) = getBinaryOperands contents     

     excludeSpecialRegisters expression 
     setDataType VARIABLE_TYPE_INT
     compileExpression leftOperand
     leftRegister <- gets currentRegister

     excludeSpecialRegisters expression
     setDataType VARIABLE_TYPE_INT
     compileExpression rightOperand
     rightRegister <- gets currentRegister
     includeAllRegisters

     putAsm
       ["mov rcx, " ++ registerName rightRegister ++ "\n",
        instruction expressionID ++ " " ++ registerName leftRegister ++ ", cl\n"]

     deallocateRegisters [rightRegister]
     setCurrentRegister leftRegister

     setDataType dataType
     insertTypeFilter VARIABLE_TYPE_INT

  where instruction expressionID =
          case expressionID of
            EXPRESSION_SHL -> "shl"
            EXPRESSION_SHR -> "shr"
            EXPRESSION_SAR -> "sar"

compileNewExpression :: Statement -> CodeTransformation ()
compileNewExpression (Statement {statementID = EXPRESSION_NEW,
                                    statementContents = (typeName:_)}) =

  do let sourceName = identifierExpressionValue (getInitialStatement typeName)
         name = map toLower sourceName
     types <- gets compileStateTypes
     symbols <- gets compileStateSymbols
     localSymbols <- gets compileStateLocalSymbols
     nameSpace <- gets compileStateNameSpace
     let t = lookupType name types
         size = typeSize t

     r1 <- reserveRegisterForFunctionCallWithPreference (functionCallRegisters !! 0)
     r2 <- reserveRegisterForFunctionCallWithPreference (functionCallRegisters !! 1)
     
     putAsm
       ["mov " ++ registerName r1 ++ ", [" ++ decorateLinkedListName name ++ "]\n",
        "mov " ++ registerName r2 ++ ", " ++ show size ++ "\n"]
     
     insertFunctionCall "new"
       [RawRegister (registerName r1) VARIABLE_TYPE_INT,
        RawRegister (registerName r2) VARIABLE_TYPE_INT]

compileTypeConversionExpression :: Statement -> CodeTransformation ()
compileTypeConversionExpression expression =

  do symbols <- gets compileStateSymbols
     localSymbols <- gets compileStateLocalSymbols
     types <- gets compileStateTypes
     let operand = getUnaryOperand (statementContents expression)
     
     sourceType <- gets getDataType
     
     case statementID expression of
       EXPRESSION_INT ->
         do setDataType VARIABLE_TYPE_INT                
            compileExpression operand
            setDataType sourceType
            insertTypeFilter VARIABLE_TYPE_INT
       EXPRESSION_FLOAT ->
         do setDataType VARIABLE_TYPE_FLOAT                
            compileExpression operand
            setDataType sourceType
            insertTypeFilter VARIABLE_TYPE_FLOAT
            
       EXPRESSION_STR ->
         do setDataType VARIABLE_TYPE_STRING     
            compileExpression operand
            setDataType sourceType
            insertTypeFilter VARIABLE_TYPE_STRING

compileTypeListExpression :: Statement -> CodeTransformation ()
compileTypeListExpression expression =
  case statementID expression of
    EXPRESSION_FIRST ->
      do let operand = getUnaryOperand (statementContents expression)
             sourceTypeName = identifierExpressionValue (getInitialStatement operand)
             typeName = map toLower sourceTypeName
             linkedListName = decorateLinkedListName typeName
         r1 <- reserveRegisterForFunctionCallWithPreference (functionCallRegisters !! 0)
         putAsm
           ["mov " ++ registerName r1 ++ ", [" ++ linkedListName ++ "]\n"]
         insertFunctionCall "first"
           [RawRegister (registerName r1) VARIABLE_TYPE_INT]
    EXPRESSION_LAST ->
      do let operand = getUnaryOperand (statementContents expression)
             sourceTypeName = identifierExpressionValue (getInitialStatement operand)
             typeName = map toLower sourceTypeName
             linkedListName = decorateLinkedListName typeName
         r1 <- reserveRegisterForFunctionCallWithPreference (functionCallRegisters !! 0)
         putAsm
           ["mov " ++ registerName r1 ++ ", [" ++ linkedListName ++ "]\n"]
         insertFunctionCall "last"
           [RawRegister (registerName r1) VARIABLE_TYPE_INT]
    EXPRESSION_BEFORE ->
      do let operand = getUnaryOperand (statementContents expression)
         insertFunctionCall "before"
           [operand]
    EXPRESSION_AFTER ->
      do let operand = getUnaryOperand (statementContents expression)
         insertFunctionCall "after"
           [operand]    
 
compileStringExpression :: Statement -> CodeTransformation ()
compileStringExpression (Statement {statementID = EXPRESSION_ADD,
                                    statementContents = contents}) =

        do dataType <- gets getDataType
           symbols <- gets compileStateSymbols
           localSymbols <- gets compileStateLocalSymbols
           nameSpace <- gets compileStateNameSpace
           types <- gets compileStateTypes

           let (leftOperand, rightOperand) = getBinaryOperands contents
               insertStringFree register =
                   do r1 <- reserveRegisterForFunctionCallWithPreference (functionCallRegisters !! 0)
                      putAsm
                        ["mov " ++ registerName r1 ++ ", " ++ registerName register ++ "\n"]
                      insertFunctionCall "free_string"
                        [RawRegister (registerName r1) VARIABLE_TYPE_STRING]
                      unused <- gets currentRegister
                      deallocateRegisters [unused]

           setDataType VARIABLE_TYPE_STRING
           compileExpression leftOperand
           leftRegister <- gets currentRegister

           setDataType VARIABLE_TYPE_STRING
           compileExpression rightOperand
           rightRegister <- gets currentRegister

           r1 <- reserveRegisterForFunctionCallWithPreference (functionCallRegisters !! 0)
           r2 <- reserveRegisterForFunctionCallWithPreference (functionCallRegisters !! 1)

           putAsm
             ["mov " ++ registerName r1 ++ ", " ++ registerName leftRegister ++ "\n",
              "mov " ++ registerName r2 ++ ", " ++ registerName rightRegister ++ "\n"]

           insertFunctionCall "concatenate_strings"
             [RawRegister (registerName r1) VARIABLE_TYPE_STRING,
              RawRegister (registerName r2) VARIABLE_TYPE_STRING]

           register <- gets currentRegister

           if isLeakyExpression symbols localSymbols types nameSpace leftOperand || getExpressionType leftOperand symbols localSymbols types /= VARIABLE_TYPE_STRING
           then insertStringFree leftRegister
           else return ()

           if isLeakyExpression symbols localSymbols types nameSpace rightOperand || getExpressionType rightOperand symbols localSymbols types /= VARIABLE_TYPE_STRING
           then insertStringFree rightRegister
           else return ()
           
           deallocateRegisters [leftRegister, rightRegister]

           let restoreCPUContext = do setCurrentRegister register
                                      setDataType dataType

           if dataType /= VARIABLE_TYPE_STRING
           then do allocateRegister
                   tempRegister <- gets currentRegister
                   putAsm
                     ["mov " ++ registerName tempRegister ++ ", " ++ registerName register ++ "\n"]
                   restoreCPUContext
                   insertTypeFilter VARIABLE_TYPE_STRING
                   insertStringFree tempRegister
                   deallocateRegisters [tempRegister]
           else return ()

           restoreCPUContext

compileIntRelationalExpression :: Statement -> CodeTransformation ()
compileIntRelationalExpression (Statement {statementID = expressionID,
                                        statementContents = contents}) =

  do dataType <- gets getDataType
     symbols <- gets compileStateSymbols
     localSymbols <- gets compileStateLocalSymbols
     nameSpace <- gets compileStateNameSpace
     let (leftOperand, rightOperand) = getBinaryOperands contents

     setDataType VARIABLE_TYPE_INT
     compileExpression leftOperand
     leftRegister <- gets currentRegister

     setDataType VARIABLE_TYPE_INT
     compileExpression rightOperand
     rightRegister <- gets currentRegister

     allocateRegister
     trueRegister <- gets currentRegister
     
     allocateRegister
     register <- gets currentRegister
     
     putAsm
       ["mov " ++ registerName trueRegister ++ ", 1\n",
        "mov " ++ registerName register ++ ", 0\n",
        "cmp " ++ registerName leftRegister ++ ", " ++ registerName rightRegister ++ "\n",
        "cmov" ++ condition1 ++ " " ++ registerName register ++ ", " ++ registerName trueRegister ++ "\n",
        if isDoubleCondition
        then "cmov" ++ condition2 ++ " " ++ registerName register ++ ", " ++ registerName trueRegister ++ "\n"
        else ""]

     deallocateRegisters [leftRegister,rightRegister,trueRegister]
       
     setDataType dataType
     insertTypeFilter VARIABLE_TYPE_INT

  where isDoubleCondition = expressionID `elem` [EXPRESSION_GREATER_THAN_OR_EQUAL_TO, EXPRESSION_LESS_THAN_OR_EQUAL_TO]
        condition1 = case expressionID of
                       EXPRESSION_EQUAL_TO -> "z"
                       EXPRESSION_NOT_EQUAL_TO -> "nz"
                       EXPRESSION_GREATER_THAN -> "g"
                       EXPRESSION_LESS_THAN -> "l"
                       EXPRESSION_GREATER_THAN_OR_EQUAL_TO -> "g"
                       EXPRESSION_LESS_THAN_OR_EQUAL_TO -> "l"
                       _ -> ""
        condition2 = case expressionID of
                       EXPRESSION_GREATER_THAN_OR_EQUAL_TO -> "z"
                       EXPRESSION_LESS_THAN_OR_EQUAL_TO -> "z"
                       _ -> ""

compileFloatRelationalExpression :: Statement -> CodeTransformation ()
compileFloatRelationalExpression (Statement {statementID = expressionID,
                                        statementContents = contents}) =

        do dataType <- gets getDataType
           symbols <- gets compileStateSymbols
           localSymbols <- gets compileStateLocalSymbols
           nameSpace <- gets compileStateNameSpace
           let (leftOperand, rightOperand) = getBinaryOperands contents
           
           setDataType VARIABLE_TYPE_FLOAT
           compileExpression leftOperand

           setDataType VARIABLE_TYPE_FLOAT
           compileExpression rightOperand
           
           allocateRegister
           trueRegister <- gets currentRegister

           allocateRegister
           register <- gets currentRegister

           deallocateFloatRegister
           deallocateFloatRegister

           putAsm
             ["mov " ++ registerName trueRegister ++ ", 1\n",
              "mov " ++ registerName register ++ ", 0\n",
              "fcomip st0, st1\n",
              "fwait\n",
              "cmov" ++ condition1 ++ " " ++ registerName register ++ ", " ++ registerName trueRegister ++ "\n",
              if isDoubleCondition
              then "cmov" ++ condition2 ++ " " ++ registerName register ++ ", " ++ registerName trueRegister ++ "\n"
              else "",
              "fstp st0\n"]

           deallocateRegisters [trueRegister]

           setDataType dataType
           insertTypeFilter VARIABLE_TYPE_INT

        where isDoubleCondition = expressionID `elem` [EXPRESSION_GREATER_THAN_OR_EQUAL_TO, EXPRESSION_LESS_THAN_OR_EQUAL_TO]
              condition1 = case expressionID of
                                EXPRESSION_EQUAL_TO -> "z"
                                EXPRESSION_NOT_EQUAL_TO -> "nz"
                                EXPRESSION_GREATER_THAN -> "b"
                                EXPRESSION_LESS_THAN -> "a"
                                EXPRESSION_GREATER_THAN_OR_EQUAL_TO -> "b"
                                EXPRESSION_LESS_THAN_OR_EQUAL_TO -> "a"
                                _ -> ""
              condition2 = case expressionID of
                               EXPRESSION_GREATER_THAN_OR_EQUAL_TO -> "z"
                               EXPRESSION_LESS_THAN_OR_EQUAL_TO -> "z"
                               _ -> ""

compileStringRelationalExpression :: Statement -> CodeTransformation ()
compileStringRelationalExpression (Statement {statementID = expressionID,
                                              statementContents = contents}) =

        do dataType <- gets getDataType
           symbols <- gets compileStateSymbols
           localSymbols <- gets compileStateLocalSymbols
           nameSpace <- gets compileStateNameSpace
           let (leftOperand, rightOperand) = getBinaryOperands contents

           allocateRegister
           trueRegister <- gets currentRegister
           
           allocateRegister
           register <- gets currentRegister

           putAsm
             ["mov " ++ registerName trueRegister ++ ", 1\n",
              "mov " ++ registerName register ++ ", 0\n"]
           insertFunctionCall "compare_strings"
             [leftOperand, rightOperand]

           result <- gets currentRegister
           deallocateRegisters [result]

           putAsm
             ["cmp eax, 0\n",
              "cmov" ++ condition1 ++ " " ++ registerName register ++ ", " ++ registerName trueRegister ++ "\n",
              if isDoubleCondition
              then "cmov" ++ condition2 ++ " " ++ registerName register ++ ", " ++ registerName trueRegister ++ "\n"
              else ""]

           deallocateRegisters [trueRegister]
           
           setCurrentRegister register

           setDataType dataType
           insertTypeFilter VARIABLE_TYPE_INT

        where isDoubleCondition = expressionID `elem` [EXPRESSION_GREATER_THAN_OR_EQUAL_TO, EXPRESSION_LESS_THAN_OR_EQUAL_TO]
              condition1 = case expressionID of
                               EXPRESSION_EQUAL_TO -> "z"
                               EXPRESSION_NOT_EQUAL_TO -> "nz"
                               EXPRESSION_GREATER_THAN -> "g"
                               EXPRESSION_LESS_THAN -> "l"
                               EXPRESSION_GREATER_THAN_OR_EQUAL_TO -> "g"
                               EXPRESSION_LESS_THAN_OR_EQUAL_TO -> "l"
                               _ -> ""
              condition2 = case expressionID of
                               EXPRESSION_GREATER_THAN_OR_EQUAL_TO -> "z"
                               EXPRESSION_LESS_THAN_OR_EQUAL_TO -> "z"
                               _ -> ""

compileCustomRelationalExpression :: Statement -> CodeTransformation ()
compileCustomRelationalExpression (Statement {statementID = expressionID,
                                        statementContents = contents}) =

  do symbols <- gets compileStateSymbols
     localSymbols <- gets compileStateLocalSymbols
     nameSpace <- gets compileStateNameSpace
     let (leftOperand, rightOperand) = getBinaryOperands contents
     
     setDataType VARIABLE_TYPE_INT
     compileExpression leftOperand
     leftRegister <- gets currentRegister

     setDataType VARIABLE_TYPE_INT
     compileExpression rightOperand
     rightRegister <- gets currentRegister

     allocateRegister
     trueRegister <- gets currentRegister

     allocateRegister
     register <- gets currentRegister

     putAsm
       ["mov " ++ registerName trueRegister ++ ", 1\n"]        

     putAsm
       ["mov " ++ registerName register ++ ", 0\n",
        "cmp " ++ registerName leftRegister ++ ", " ++ registerName rightRegister ++ "\n",
        "cmov" ++ condition ++ " " ++ registerName register ++ ", " ++ registerName trueRegister ++ "\n"]

     deallocateRegisters [leftRegister,rightRegister,trueRegister]
     setCurrentRegister register

     where condition =
             case expressionID of
               EXPRESSION_EQUAL_TO -> "z"
               EXPRESSION_NOT_EQUAL_TO -> "nz"

compileFieldAccessExpression :: Bool -> Statement -> CodeTransformation ()
compileFieldAccessExpression dereference expression =

  do symbols <- gets compileStateSymbols
     localSymbols <- gets compileStateLocalSymbols
     nameSpace <- gets compileStateNameSpace
     types <- gets compileStateTypes
     debug <- gets (optionDebug . configOptions . compileStateConfig)
     destType <- gets getDataType
     
     let (leftOperand,rightOperand) = getBinaryOperands (statementContents expression)
         dataType = getExpressionType expression symbols localSymbols types
     
     case statementID leftOperand of
       EXPRESSION_IDENTIFIER ->
         do let sourceName = identifierExpressionValue (getInitialStatement leftOperand)
                name = map toLower (removeTypeTag sourceName)
                sourceFieldName = identifierExpressionValue (getInitialStatement rightOperand)
                fieldName = map toLower (removeTypeTag sourceFieldName)
                localVariableContainer = Map.lookup name localSymbols
                globalVariableContainer = Map.lookup name symbols
                variableContainer =
                  if localVariableContainer /= Nothing
                  then localVariableContainer
                  else if globalVariableContainer /= Nothing
                       then globalVariableContainer
                       else Nothing
                variable = fromJust variableContainer
                dataType = case variable of
                             Variable {variableType = t} -> t
                             LocalAutomaticVariable {localAutomaticVariableType = t} -> t
                typeContainer = Map.lookup (map toLower (customTypeName dataType)) types
                type_ = fromJust typeContainer
                typeFields = typeSymbols type_
                fieldContainer = Map.lookup fieldName typeFields
                field = fromJust fieldContainer

            allocateRegister
            register <- gets currentRegister

            putAsm
              ["mov " ++ registerName register ++ ", [" ++ decorateVariableName name symbols localSymbols nameSpace ++ "]\n"]

            exceptionCheck register debug leftOperand
            setCurrentRegister register
                      
            if fieldOffset field /= 0
            then putAsm
                   ["add " ++ registerName register ++ ", " ++ (show (8 * fieldOffset field)) ++ "\n"]
            else return ()

       EXPRESSION_FIELD_ACCESS ->
         do let leftOperandType = getExpressionType leftOperand symbols localSymbols types
                sourceFieldName = identifierExpressionValue (getInitialStatement rightOperand)
                fieldName = map toLower (removeTypeTag sourceFieldName)
                typeContainer = Map.lookup (map toLower (customTypeName leftOperandType)) types
                type_ = fromJust typeContainer
                typeFields = typeSymbols type_
                fieldContainer = Map.lookup fieldName typeFields
                field = fromJust fieldContainer
                fieldDataType = fieldType field

            setDataType (VARIABLE_TYPE_CUSTOM "Null")
            compileFieldAccessExpression True leftOperand

            register <- gets currentRegister

            exceptionCheck register debug leftOperand
            setCurrentRegister register

            if fieldOffset field /= 0
            then putAsm
                   ["add " ++ registerName register ++ ", " ++ (show (8 * fieldOffset field)) ++ "\n"]
            else return ()

       EXPRESSION_FUNCTION_CALL ->
         do let (identifier,arguments) = getFunctionCallOperands (statementContents leftOperand)
                sourceName = identifierExpressionValue (getInitialStatement identifier)
                numArguments = length arguments
                name = removeTypeTag (map toLower sourceName)
                variableContainer = Map.lookup name symbols
                variable = fromJust variableContainer
                Array {arrayType = dataType, arrayNumDimensions = numDimensions} = variable
                sourceFieldName = identifierExpressionValue (getInitialStatement rightOperand)
                fieldName = map toLower (removeTypeTag sourceFieldName)
                typeContainer = Map.lookup (map toLower (customTypeName (targetType dataType))) types
                type_ = fromJust typeContainer
                typeFields = typeSymbols type_
                fieldContainer = Map.lookup fieldName typeFields
                field = fromJust fieldContainer
                fieldDataType = fieldType field

            setDataType (getExpressionType leftOperand symbols localSymbols Map.empty)
            insertArrayAccess name arguments True
            register <- gets currentRegister

            exceptionCheck register debug leftOperand
            setCurrentRegister register

            if fieldOffset field /= 0
            then putAsm
                   ["add " ++ registerName register ++ ", " ++ (show (8 * fieldOffset field)) ++ "\n"]
            else return ()               

     setDataType destType
     
     if dereference
     then case dataType of
            VARIABLE_TYPE_INT ->
              do register <- gets currentRegister
                 putAsm
                   ["mov " ++ registerName register ++ ", [" ++ registerName register ++ "]\n"]
                 insertTypeFilter VARIABLE_TYPE_INT
            VARIABLE_TYPE_FLOAT ->
              do register <- gets currentRegister
                 allocateFloatRegister
                 putAsm
                   ["mov " ++ registerName register ++ ", [" ++ registerName register ++ "]\n",
                    "sub rsp, 8\n",
                    "mov [rsp], " ++ registerName register ++ "\n",
                    "fld qword [rsp]\n",
                    "fwait\n",
                    "add rsp, 8\n"]
                 deallocateRegisters [register]
                 insertTypeFilter VARIABLE_TYPE_FLOAT
            VARIABLE_TYPE_STRING ->
              do register <- gets currentRegister
                 putAsm
                   ["mov " ++ registerName register ++ ", [" ++ registerName register ++ "]\n"]
                 insertTypeFilter VARIABLE_TYPE_STRING
            VARIABLE_TYPE_CUSTOM {customTypeName = typeName} ->
              do register <- gets currentRegister
                 putAsm
                   ["mov " ++ registerName register ++ ", [" ++ registerName register ++ "]\n"]
                 insertTypeFilter (VARIABLE_TYPE_CUSTOM typeName)
     else return ()

  where exceptionCheck register debug statement =
          do anticipateFatalError statement
             if debug
             then do nextLabelID
                     skipLabelID <- gets compileStateLabelID
                     putAsm
                       ["cmp " ++ registerName register ++ ", 0\n",
                        "jne " ++ generateLabelName skipLabelID ++ "\n"]
                     insertFunctionCall "fatal_error"
                       [createMinimalStatement EXPRESSION_STRING_CONSTANT [StringConstantExpression runtimeErrorNullPointerException]]
                            
                     putAsm
                       [generateLabelName skipLabelID ++ ":\n\n"]
                         
             else return ()

insertTypeFilter :: VariableType -> CodeTransformation ()
insertTypeFilter sourceType =

        do destType <- gets getDataType
           symbols <- gets compileStateSymbols
           localSymbols <- gets compileStateLocalSymbols
           nameSpace <- gets compileStateNameSpace
           let types = (destType, sourceType)

           case types of

                (VARIABLE_TYPE_INT, VARIABLE_TYPE_INT) ->

                  do return ()

                (VARIABLE_TYPE_INT, VARIABLE_TYPE_FLOAT) ->

                  do allocateRegister
                     register <- gets currentRegister
                     putAsm
                       ["sub rsp, 8\n",
                        "fistp qword [rsp]\n",
                        "fwait\n",
                        "pop " ++ registerName register ++ "\n"]
                     deallocateFloatRegister

                (VARIABLE_TYPE_INT, VARIABLE_TYPE_STRING) ->

                  do register <- gets currentRegister

                     r1 <- reserveRegisterForFunctionCallWithPreference (functionCallRegisters !! 0)

                     putAsm
                       ["mov " ++ registerName r1 ++ ", " ++ registerName register ++ "\n"]

                     insertFunctionCall "convert_string_to_int"
                       [RawRegister (registerName r1) VARIABLE_TYPE_STRING]

                (VARIABLE_TYPE_FLOAT, VARIABLE_TYPE_INT) ->

                  do register <- gets currentRegister
                     allocateFloatRegister

                     putAsm
                       ["push " ++ registerName register ++ "\n",
                        "fild qword [rsp]\n",
                        "fwait\n",
                        "add rsp, 8\n"]
                     deallocateRegisters [register]

                (VARIABLE_TYPE_FLOAT, VARIABLE_TYPE_FLOAT) ->

                  return ()

                (VARIABLE_TYPE_FLOAT, VARIABLE_TYPE_STRING) ->

                  do register <- gets currentRegister

                     r1 <- reserveRegisterForFunctionCallWithPreference (functionCallRegisters !! 0)
                     putAsm
                       ["mov " ++ registerName r1 ++ ", " ++ registerName register ++ "\n"]

                     insertFunctionCall "convert_string_to_float"
                       [RawRegister (registerName r1) VARIABLE_TYPE_STRING]

                     result <- gets currentRegister
                     allocateFloatRegister
                    
                     putAsm
                       ["push " ++ registerName result ++ "\n",
                        "fld qword [rsp]\n",
                        "pop rax\n"]

                     deallocateRegisters [register,result]

                (VARIABLE_TYPE_STRING, VARIABLE_TYPE_INT) ->

                  do register <- gets currentRegister
                     
                     r1 <- reserveRegisterForFunctionCallWithPreference (functionCallRegisters !! 0)

                     putAsm
                       ["mov " ++ registerName r1 ++ ", " ++ registerName register ++ ";mmm2\n"]

                     insertFunctionCall "convert_int_to_string"
                       [RawRegister (registerName r1) VARIABLE_TYPE_INT]

                     result <- gets currentRegister

                     putAsm
                       ["mov " ++ registerName register ++ ", " ++ registerName result ++ "\n"]

                     deallocateRegisters [result]

                     setCurrentRegister register

                (VARIABLE_TYPE_STRING, VARIABLE_TYPE_FLOAT) ->

                  do state <- get
                     let offset = (cpuContextOffset . compileStateRegisters) state
                         alignment = if odd offset
                                     then 8
                                     else 16
                     deallocateFloatRegister
                     putAsm
                       ["fstp qword [rsp]\n",
                        "fwait\n",
                        "movq xmm0, [rsp]\n"]
                     
                     insertFunctionCall "convert_float_to_string" []
                       
                (VARIABLE_TYPE_STRING, VARIABLE_TYPE_STRING) ->

                  do return ()
                     
                (VARIABLE_TYPE_STRING, VARIABLE_TYPE_CUSTOM {}) ->

                  do register <- gets currentRegister

                     r1 <- reserveRegisterForFunctionCallWithPreference (functionCallRegisters !! 0)

                     putAsm
                       ["mov " ++ registerName r1 ++ ", " ++ registerName register ++ "\n"]

                     insertFunctionCall "convert_pointer_to_string"
                       [RawRegister (registerName r1) VARIABLE_TYPE_INT]

                     result <- gets currentRegister
                     putAsm ["mov " ++ registerName register ++ ", " ++ registerName result ++ "\n"]
                     deallocateRegisters [result]
                     setCurrentRegister register
                     
                (_, _) ->

                  do return ()

putDirectivesAndFunctionDeclarations :: CodeTransformation ()
putDirectivesAndFunctionDeclarations =
  do symbols <- gets compileStateSymbols
     bucket <- getAsmBucket

     setAsmBucket directives_

#if WINDOWS==1
     putAsmGeneric
       ["format MS64 COFF\n\n"]
       ["default rel\n\n"]
#elif LINUX==1 || MAC_OS==1
     putAsmGeneric
       ["format ELF64\n\n"]
       ["default rel\n\n"]
#endif

     putInternalFunctionDeclarations
     putExternalFunctionDeclarations symbols

     setAsmBucket bucket     

     where putInternalFunctionDeclarations =

             do putAsmGeneric ["public " ++ osFunctionPrefix ++ "main\n\n"] ["global " ++ osFunctionPrefix ++ "main\n\n"]

           putExternalFunctionDeclarations symbols =

             do putAsmGeneric
                  ["extrn " ++ osFunctionPrefix ++ "bb_init_string\n",
                   "extrn " ++ osFunctionPrefix ++ "bb_free_string\n",
                   "extrn " ++ osFunctionPrefix ++ "exit\n",
                   "extrn " ++ osFunctionPrefix ++ "strlen\n",
                   "extrn " ++ osFunctionPrefix ++ "free\n",
                   "extrn " ++ osFunctionPrefix ++ "dpiHack\n"]
                  ["extern " ++ osFunctionPrefix ++ "bb_init_string\n",
                   "extern " ++ osFunctionPrefix ++ "bb_free_string\n",
                   "extern " ++ osFunctionPrefix ++ "exit\n",
                   "extern " ++ osFunctionPrefix ++ "strlen\n",
                   "extern " ++ osFunctionPrefix ++ "free\n",
                   "extern " ++ osFunctionPrefix ++ "dpiHack\n"]

                mapM_ externFunctionDeclaration (map snd (Map.toList symbols))

putGlobalData :: CodeTransformation ()
putGlobalData =

        do symbols <- gets compileStateSymbols
           types <- gets compileStateTypes
           strings <- gets compileStateStrings

           bucket <- getAsmBucket
         
           setAsmBucket globals_
           putDataSectionHeader
           putConstantDeclarations
           putNewline
           putVariableDeclarations symbols strings
           putLinkedListDeclarations types
           putNewline

           setAsmBucket bucket

        where putDataSectionHeader =
#if LINUX==1 || MAC_OS==1
                do putAsmGeneric ["section '.data' writable\n\n"] ["SECTION .data\n\n"]
#elif WINDOWS==1
                do putAsmGeneric ["section '.data' writable align 16\n\n"] ["SECTION .data\n\n"]
#endif

              putConstantDeclarations =

                do putAsm
                     ["DATA_TYPE_INT equ 1\n",
                      "DATA_TYPE_FLOAT equ 2\n",
                      "DATA_TYPE_STRING equ 3\n",
                      "OUT_OF_DATA equ -1\n"]

              putVariableDeclarations symbols strings =

                do putAsm
                     (["align 8\n"] ++ (map (variableDeclaration symbols strings) (filter (\v -> isConst v || isArray v || isIntrinsicTypeVariable v || isCustomTypeVariable v) (Map.elems symbols))))
              putLinkedListDeclarations types =

                do mapM_ linkedListDeclaration (map snd (Map.toList types))

putLinkedListInit :: CodeTransformation ()
putLinkedListInit =
  do types <- gets compileStateTypes
     let typeList = (map snd (Map.toList types))
     mapM_ linkedListInit typeList

linkedListInit :: Symbol -> CodeTransformation ()
linkedListInit type_ =
  do r2 <- reserveRegisterForFunctionCallWithPreference (functionCallRegisters !! 1)
     putAsm
       ["mov " ++ registerName r2 ++ ", " ++ decorateTypeStringOffsetsName (typeName type_) ++ "\n"]
     insertFunctionCall "create_linked_list"
       [createMinimalStatement EXPRESSION_INT_CONSTANT [IntConstantExpression (typeSize type_)],
        RawRegister (registerName r2) VARIABLE_TYPE_INT]
     register <- gets currentRegister
     putAsm
       ["\nmov [" ++ decorateLinkedListName (typeName type_) ++ "], " ++ registerName register ++ "\n"]
     deallocateRegisters [register]

putLinkedListFinal :: CodeTransformation ()
putLinkedListFinal =
  do types <- gets compileStateTypes
     let typeList = (map snd (Map.toList types))
     mapM_ linkedListFinal typeList

linkedListFinal :: Symbol -> CodeTransformation ()
linkedListFinal type_ =
  do r1 <- reserveRegisterForFunctionCallWithPreference (functionCallRegisters !! 0)
     putAsm
       ["mov " ++ registerName r1 ++ ", [" ++ decorateLinkedListName (typeName type_) ++ "]\n"]
     insertFunctionCall "destroy_linked_list"
       [RawRegister (registerName r1) VARIABLE_TYPE_INT]
     register <- gets currentRegister
     deallocateRegisters [register]

putDataSentinel :: CodeTransformation ()
putDataSentinel =
  do bucket <- getAsmBucket
     setAsmBucket data_
     putAsm
       ["dq OUT_OF_DATA\n\n"]
     setAsmBucket bucket

putEachTypeStringOffsets :: CodeTransformation ()
putEachTypeStringOffsets =
  do bucket <- getAsmBucket
     setAsmBucket data_
     types <- gets compileStateTypes
     mapM_ putTypeStringOffsets (Foldable.toList types)
     setAsmBucket bucket

putTypeStringOffsets :: Symbol -> CodeTransformation ()
putTypeStringOffsets type_ =
  do let symbols = typeSymbols type_
         stringFields = filter (\f -> fieldType f == VARIABLE_TYPE_STRING) (Foldable.toList symbols)
     putAsm
       [decorateTypeStringOffsetsName (typeName type_) ++ ":\n\n"]
     mapM_ putTypeStringOffset stringFields
     putDataSentinel
     putNewline
  where putTypeStringOffset field =
          putAsm
            ["dq " ++ show (fieldOffset field) ++ "\n"]

putInts :: CodeTransformation ()
putInts = 
  do bucket <- getAsmBucket
     setAsmBucket globals_
     ints <- gets compileStateInts
     putIntDeclarations ints
     setAsmBucket bucket
     
     where putIntDeclarations ints =
             do putAsm (map intDeclaration (Map.assocs ints))

putFloats :: CodeTransformation ()
putFloats = 
  do bucket <- getAsmBucket
     setAsmBucket globals_
     floats <- gets compileStateFloats
     putFloatDeclarations floats
     setAsmBucket bucket
     
     where putFloatDeclarations floats =
             do putAsmGeneric (map fasmFloatDeclaration (Map.assocs floats)) (map nasmFloatDeclaration (Map.assocs floats))

putStrings :: CodeTransformation ()
putStrings = 
  do bucket <- getAsmBucket
     setAsmBucket globals_
     strings <- gets compileStateStrings
     putStringDeclarations strings
     setAsmBucket bucket
     
     where putStringDeclarations strings =
             do putAsm
                  (["align 8\n", "NULL_STRING db 0\n", "align 8\n"] ++ (map stringDeclaration (Map.assocs strings)))

#if LINUX==1 || MAC_OS==1
putAuxiliaryCode :: CodeTransformation ()
putAuxiliaryCode =
  do bucket <- getAsmBucket
     setAsmBucket functions_
     {--putAsm
       ["" ++ bbFunctionPrefix ++ "init_string:\n\n",
        "push rbp\n",
        "mov rax, NULL_STRING\n",
        "mov [rdi], rax\n",
        "pop rbp\n",
        "ret\n\n"]

     putAsm
       [bbFunctionPrefix ++ "free_string:\n\n",
        "push rbp\n",
        "mov rbp, rsp\n",
        "push rbx\n",
        "push r12\n",
        "push r13\n",
        "push r14\n",
        "push r15\n",
        "mov rbx, rdi\n",
        "cmp rbx, 0\n",
        "jz .null_or_empty_string\n",
        "sub rsp, 8\n",
        "call [" ++ osFunctionPrefix ++ "strlen wrt ..got]\n",
        "add rsp, 8\n",
        "cmp rax, 0\n",
        "jle .null_or_empty_string\n",
        "mov rdi, rbx\n",
        "mov rax, 0\n",
        "sub rsp, 8\n",
        "call [" ++ osFunctionPrefix ++ "free wrt ..got]\n",
        "add rsp, 8\n",
        ".null_or_empty_string:\n",
        "pop r15\n",
        "pop r14\n",
        "pop r13\n",
        "pop r12\n",
        "pop rbx\n",
        "pop rbp\n",
        "ret\n\n"]
     --}
     setAsmBucket bucket

#elif WINDOWS==1
putAuxiliaryCode :: CodeTransformation ()
putAuxiliaryCode =
  do bucket <- getAsmBucket
     setAsmBucket functions_
     putAsm
       ["" ++ bbFunctionPrefix ++ "init_string:\n\n",
        "mov rax, NULL_STRING\n",
        "mov [rcx], rax\n",
        "ret\n\n"]
     putAsm
       [bbFunctionPrefix ++ "free_string:\n\n",
        "push rbp\n",
        "mov rbp, rsp\n",
        "push rbx\n",
        "push rsi\n",
        "push rdi\n",
        "push r12\n",
        "push r13\n",
        "push r14\n",
        "push r15\n",
        "mov rbx, rcx\n",
        "cmp rbx, 0\n",
        "jz .null_or_empty_string\n",
        "sub rsp, 8\n",
        "call [" ++ osFunctionPrefix ++ "strlen wrt ..got]\n",
        "add rsp, 8\n",
        "cmp rax, 0\n",
        "jle .null_or_empty_string\n",
        "mov rcx, rbx\n",
        "mov rax, 0\n",
        "sub rsp, 8\n",
        "call [" ++ osFunctionPrefix ++ "free wrt ..got]\n",
        "add rsp, 8\n",
        ".null_or_empty_string:\n",

        "pop r15\n",
        "pop r14\n",
        "pop r13\n",
        "pop r12\n",
        "pop rdi\n",
        "pop rsi\n",
        "pop rbx\n",
        "pop rbp\n",
        "ret\n\n"]
     setAsmBucket bucket
#endif

putBasicEpilogue :: CodeTransformation ()
putBasicEpilogue =
  do
#if LINUX==1 || MAC_OS==1
     putAsm
       ["call .cleanup\n",
        "mov rax, 0\n",
        "mov rdi, 0\n",
        "call [" ++ osFunctionPrefix ++ "exit wrt ..got]\n\n"]
#elif WINDOWS==1
     putAsm
       ["call .cleanup\n",
        "mov rcx, 0\n",
        "sub rsp, 32\n",
        "call [" ++ osFunctionPrefix ++ "exit wrt ..got]\n\n"]
#endif

putCleanup :: CodeTransformation ()
putCleanup =

        do bucket <- getAsmBucket
           setAsmBucket code_
           symbols <- gets compileStateSymbols
           localSymbols <- gets compileStateLocalSymbols
           putAsm
             [".cleanup:\n\n",
              "push rbp\n"]
           putAsm (stringCleanup symbols)
           arrayCleanup symbols localSymbols
           putAsm
             ["pop rbp\n",
              "ret\n\n"]
           setAsmBucket bucket

        where stringCleanup symbols = map (stringFree symbols) (filter (\ v -> variableType v == VARIABLE_TYPE_STRING) (filter isIntrinsicTypeVariable (Map.elems symbols)))
              arrayCleanup symbols localSymbols = mapM_ (arrayFree symbols localSymbols) (filter (\ v -> isArray v) (Map.elems symbols))

externFunctionDeclaration :: Symbol -> CodeTransformation ()
externFunctionDeclaration (Function {functionName = name,functionRawName = rawName,functionOrigin = FUNCTION_ORIGIN_STANDARD}) =
  do symbols <- gets compileStateSymbols
     if standardFunctionIsUsed rawName symbols
     then putAsmGeneric ["extrn " ++ name ++ "\n"] ["extern " ++ name ++ "\n"]
     else return ()

externFunctionDeclaration (Function {functionName = name,functionOrigin = FUNCTION_ORIGIN_SYSTEM}) =
  putAsmGeneric ["extrn " ++ name ++ "\n"] ["extern " ++ name ++ "\n"]

externFunctionDeclaration _ = return ()

intToHexString :: Int -> [Char]
intToHexString integer =
  if isolateRest integer /= 0
  then  (intToHexString . shiftRight4Bits) integer Prelude.++ (nibbleToHexDigit . isolateNibble) integer
  else (nibbleToHexDigit . isolateNibble) integer
  where shiftRight4Bits i = uShiftR i 4
        isolateNibble i = i .&. 0xF
        isolateRest i = i .&. 0xFFFFFFFFFFFFFFF0
        nibbleToHexDigit n =
          case n of
            0 -> "0"
            1 -> "1"
            2 -> "2"
            3 -> "3"
            4 -> "4"
            5 -> "5"
            6 -> "6"
            7 -> "7"
            8 -> "8"
            9 -> "9"
            10 -> "A"
            11 -> "B"
            12 -> "C"
            13 -> "D"
            14 -> "E"
            15 -> "F"

intToBinString :: Int -> [Char]
intToBinString integer =
  if isolateRest integer /= 0
  then (intToBinString . shiftRight1Bit) integer Prelude.++ (bitToBinDigit . isolateBit) integer
  else (bitToBinDigit . isolateBit) integer
  where shiftRight1Bit i = uShiftR i 1
        isolateBit i = i .&. 1
        isolateRest i = i .&. (-2)
        bitToBinDigit n =
          case n of
            0 -> "0"
            1 -> "1"


putDebugInfo :: CodeTransformation ()
putDebugInfo =
  do debug <- gets (optionDebug . configOptions . compileStateConfig)
     if debug
     then do mapM_ putSectionHeader [(debug_info_,"debug_info"),(debug_abbrev_,"debug_abbrev"),(debug_line_,"debug_line"),(debug_str_,"debug_str")]
             
             putCompilationUnitHeader
             putDWARFAbbreviations
             putDWARFDIEs
             putDWARFStrings
             
             
     else return ()
     where putSectionHeader (bucket,name) =
             do prevBucket <- getAsmBucket
                setAsmBucket bucket
                putAsmGeneric
                  ["section '." ++ name ++ "'\n\n"] ["SECTION ." ++ name ++ "\n\n"]
                
                setAsmBucket prevBucket
              
--putDWARFCompilationUnitHeader :: CodeTransformation ()
--putDWARFCompilationUnitHeader =
--  do

attributeSpecToString :: DWARFAttributeSpec -> [Char]
attributeSpecToString spec =
  
  show (specName spec) ++ "\n" ++ show (specForm spec)

putCompilationUnitHeader :: CodeTransformation ()
putCompilationUnitHeader =
  do bucket <- getAsmBucket
     dwarfDIEs <- gets (debugInfoDIEs . compileStateDebugInfo)
     offset <- gets (debugInfoDIEOffset . compileStateDebugInfo)
     
     let unitLength = offset - fromIntegral dwarfCompilationUnitInitialLengthSize + 1 --fromIntegral (dwarfCompilationUnitHeaderSize - dwarfCompilationUnitInitialLengthSize) + offset -- (foldr (\dd a -> dwarfDIECalculateSize dd + a) 0 dwarfDIEs)

     setAsmBucket debug_info_
     putAsm ["dd " ++ show unitLength ++ "\n",
             "dw 4\n",
             "dd 0\n",
             "db 8\n"]
     setAsmBucket bucket
     
unsignedLEB128ToString :: DWARFUnsignedLEB128 -> [Char]
unsignedLEB128ToString i =
  intercalate "," (map (("0x" ++) . intToHexString) (map fromIntegral (uLEB128 i)))

signedLEB128ToString :: DWARFSignedLEB128 -> [Char]
signedLEB128ToString i =
  intercalate ", " (map (("0x" ++) . intToHexString) (map fromIntegral (sLEB128 i)))

putDWARFAbbreviation :: DWARFAbbreviation -> CodeTransformation ()
putDWARFAbbreviation abbreviation =
  do if abbreviationKey abbreviation /= "NONE"
     then do putAsm
               ["db " ++ unsignedLEB128ToString (encodeUnsignedLEB128 (abbreviationCode abbreviation)) ++ " ; abbreviation code\n",
                "db " ++ unsignedLEB128ToString (encodeUnsignedLEB128 (abbreviationTag abbreviation)) ++ " ; " ++ dwarfTagEncodingToString (abbreviationTag abbreviation) ++ "\n",
                if abbreviationHasChildren abbreviation
                then "db 1 ; DW_CHILDREN_yes\n"
                else "db 0 ; DW_CHILDREN_no\n"]
             putAsm (map attributeSpecToAsm (abbreviationSpecs abbreviation))
             putAsm ["db 0,0\n\n"]
     else return ()
  where attributeSpecToAsm spec =
          "db " ++ unsignedLEB128ToString (encodeUnsignedLEB128 (specName spec)) ++ " ; " ++ dwarfAttributeNameEncodingToString (specName spec) ++ "\n" ++
          "db " ++ unsignedLEB128ToString (encodeUnsignedLEB128 (specForm spec)) ++ " ; " ++ dwarfAttributeFormEncodingToString (specForm spec) ++ "\n"
        
putDWARFAbbreviations :: CodeTransformation ()
putDWARFAbbreviations =
  do bucket <- getAsmBucket
     
     setAsmBucket debug_abbrev_  
     mapM_ putDWARFAbbreviation indexedAbbreviations
     putAsm ["dq 0\n\n"]
     
     setAsmBucket bucket

bytesToString :: [Word8] -> [Char]
bytesToString bytes =
  intercalate ", " (map (("0x" ++) . intToHexString) (map fromIntegral bytes))

putDWARFAttribute :: DWARFAttribute -> CodeTransformation ()
putDWARFAttribute a =
  do let sizeDecl =
           case dwarfAttributeSize a of
                  1 -> "db"
                  2 -> "dw"
                  4 -> "dd"
                  8 -> "dq"
                  _ -> "db"
                  
     putAsm [sizeDecl ++ " " ++ dwarfAttributeToString a ++ "\n"]
  
dwarfAttributeToString :: DWARFAttribute -> [Char]
dwarfAttributeToString (DWARFAttributeRefStrp d) = show d
dwarfAttributeToString (DWARFAttributeAddr a) = show a
dwarfAttributeToString (DWARFAttributeBlock1 s d) = "0x" ++ intToHexString (fromIntegral s) ++ ", " ++ bytesToString d
dwarfAttributeToString (DWARFAttributeBlock2 s d) = "0x" ++ intToHexString (fromIntegral s) ++ ", " ++ bytesToString d
dwarfAttributeToString (DWARFAttributeBlock4 s d) = "0x" ++ intToHexString (fromIntegral s) ++ ", " ++ bytesToString d
dwarfAttributeToString (DWARFAttributeBlock8 s d) = "0x" ++ intToHexString (fromIntegral s) ++ ", " ++ bytesToString d
dwarfAttributeToString (DWARFAttributeBlock s d) = "0x" ++ unsignedLEB128ToString s ++ ", " ++ bytesToString d ++ "\n"
dwarfAttributeToString (DWARFAttributeData1UnsignedInt d) = show d
dwarfAttributeToString (DWARFAttributeData1SignedInt d) = show d
dwarfAttributeToString (DWARFAttributeData2UnsignedInt d) = show d
dwarfAttributeToString (DWARFAttributeData2SignedInt d) = show d
dwarfAttributeToString (DWARFAttributeData4UnsignedInt d) = show d
dwarfAttributeToString (DWARFAttributeData4SignedInt d) = show d
dwarfAttributeToString (DWARFAttributeData4Float d) = show d
dwarfAttributeToString (DWARFAttributeData8UnsignedInt d) = show d
dwarfAttributeToString (DWARFAttributeData8SignedInt d) = show d
dwarfAttributeToString (DWARFAttributeData8Double d) = show d
dwarfAttributeToString (DWARFAttributeDataUnsignedLEB128 d) = unsignedLEB128ToString d
--dwarfAttributeToString (DWARFAttributeDataSignedLEB128 _) = 
--dwarfAttributeToString (DWARFAttributeExprLoc) = 0 
dwarfAttributeToString (DWARFAtttributeFlag d) = show d
dwarfAttributeToString (DWARFAttributeLinePtr d) = show d
dwarfAttributeToString (DWARFAttributeLocListPtr d) = show d
dwarfAttributeToString (DWARFAttributeMacPtr d) = show d
dwarfAttributeToString (DWARFAttributeRangeListPtr d) = show d
dwarfAttributeToString (DWARFAttributeRef1 d) = show d
dwarfAttributeToString (DWARFAttributeRef2 d) = show d
dwarfAttributeToString (DWARFAttributeRef4 d) = show d
dwarfAttributeToString (DWARFAttributeRef8 d) = show d
dwarfAttributeToString (DWARFAttributeRefUData d) = unsignedLEB128ToString d
dwarfAttributeToString (DWARFAttributeRefAddr d) = show d
dwarfAttributeToString (DWARFAttributeRefSig8 d) = show d
dwarfAttributeToString (DWARFAttributeRefString s) = s ++ ",0"
dwarfAttributeToString (DWARFAttributeRefStrp p) = show p
dwarfAttributeToString (DWARFDIECrossReference i) = show i
dwarfAttributeToString k = error (show k)

{--
putDWARFDIE :: DWARFDIE -> CodeTransformation ()
putDWARFDIE dwarfDIE =
  do --liftIO $ putStrLn ("DIE of offst " ++ show (dwarfDIEOffset dwarfDIE))
     putAsm ["db " ++ unsignedLEB128ToString (encodeUnsignedLEB128 (dwarfDIEAbbreviationCode dwarfDIE)) ++ "\n"]
     mapM_ putDWARFAttribute (dwarfDIEAttributes dwarfDIE)
--}

putDWARFDIE :: DWARFDIE -> Int -> CodeTransformation ()
putDWARFDIE dwarfDIE depth =
  do liftIO $ putStrLn ("DIE of depth " ++ show (dwarfDIEDepth dwarfDIE))
     if dwarfDIEDepth dwarfDIE < depth
     then do return ()
             putAsm ["db 0;:LALALA\n"]
             {--state <- get
             debugInfo <- gets compileStateDebugInfo
             offset <- gets (debugInfoDIEOffset . compileStateDebugInfo)
             put state {compileStateDebugInfo =
                        debugInfo {debugInfoDIEOffset = offset + 1}}
--}
             
             
     else return ()
     putAsm ["db " ++ unsignedLEB128ToString (encodeUnsignedLEB128 (dwarfDIEAbbreviationCode dwarfDIE)) ++ "\n"]
     mapM_ putDWARFAttribute (dwarfDIEAttributes dwarfDIE)

putDWARFDIEs :: CodeTransformation ()
putDWARFDIEs =
  do bucket <- getAsmBucket
     rawDIEs <- gets (debugInfoDIEs . compileStateDebugInfo) 
     let dwarfDIEs = map snd (sortBy (comparing (dwarfDIEOffset .snd)) (Map.toAscList rawDIEs))
     
     setAsmBucket debug_info_  
     putDWARFDIEsLoop dwarfDIEs 0
     putAsm ["db 0\n"]
     setAsmBucket bucket

putDWARFDIEsLoop :: [DWARFDIE] -> Int -> CodeTransformation ()
putDWARFDIEsLoop dies depth
  | length dies > 1 =
      do putDWARFDIE (head dies) depth
         putDWARFDIEsLoop (tail dies) (dwarfDIEDepth (head dies))
  | length dies == 1 =    
      do putDWARFDIE (head dies) depth
  | otherwise =
      do return ()
  
putDWARFString :: [Char] -> CodeTransformation ()
putDWARFString string =
  do putAsm ["db " ++ show string ++ ",0\n"]
  
putDWARFStrings :: CodeTransformation ()
putDWARFStrings =
  do strings <- gets (debugInfoStrings . compileStateDebugInfo)
     bucket <- getAsmBucket
  
     setAsmBucket debug_str_
     mapM_ putDWARFString strings
     setAsmBucket bucket

linkedListDeclaration :: Symbol -> CodeTransformation ()
linkedListDeclaration (Type {typeName = name}) =
  putAsm
    [decorateLinkedListName name ++ " dq 0\n"]

linkedListDeclaration _ = return ()

variableDeclaration :: SymbolTable -> StringTable -> Symbol -> String
variableDeclaration symbols strings (Const {constName = name,
                                            constType = dataType,
                                            constValue = value}) =
  let expression = getInitialStatement (constValueExpression value)
      int = intConstantExpressionValue expression
      float = floatConstantExpressionValue expression
      string = stringConstantExpressionValue expression
      stringID = lookupString string strings in
      
  case statementID (constValueExpression value) of
    EXPRESSION_INT_CONSTANT -> decorateVariableName name symbols Map.empty NO_NAMESPACE ++ " dq " ++ (show int) ++ "\n"
    EXPRESSION_FLOAT_CONSTANT -> decorateVariableName name symbols Map.empty NO_NAMESPACE ++ " dq " ++ (show float) ++ "\n"
    EXPRESSION_STRING_CONSTANT -> decorateVariableName name symbols Map.empty NO_NAMESPACE ++ " dq SC" ++ show stringID ++ "\n "

variableDeclaration symbols strings (Variable {variableName = name,
                                       variableType = dataType}) =
        if dataType == VARIABLE_TYPE_STRING
        then decorateVariableName name symbols Map.empty NO_NAMESPACE ++ " dq NULL_STRING\n"
        else decorateVariableName name symbols Map.empty NO_NAMESPACE ++ " dq 0\n"

variableDeclaration symbols strings (Array name dataType numDimensions) =
        decorateVariableName name symbols Map.empty NO_NAMESPACE ++ " dq 0\n"

intDeclaration :: (Int,Int) -> String
intDeclaration (int,id) =
  "IC" ++ show id ++ " dq " ++ show int ++ "\n"

nasmFloatDeclaration :: (Double,Int) -> String
nasmFloatDeclaration (float,id) =
  "FC" ++ show id ++ " dq __float64__(" ++ show float ++ ")\n"

fasmFloatDeclaration :: (Double,Int) -> String
fasmFloatDeclaration (float,id) =
  "FC" ++ show id ++ " dq " ++ show float ++ "\n"

stringDeclaration :: (String, Int) -> String
stringDeclaration (string, id) =
         "SC" ++ show id ++ " db " ++ string ++ ",0\n"

stringFree :: SymbolTable -> Symbol -> String
stringFree symbols (Variable {variableName = name}) =
         if head name == '_'
         then ""
#if LINUX==1 || MAC_OS==1
         else "mov rdi, [" ++ decorateVariableName name symbols Map.empty NO_NAMESPACE ++ "]\n" ++
              "mov rax, 0\n" ++
              "call [" ++ bbFunctionPrefix ++ "free_string wrt ..got]\n\n"
#elif WINDOWS==1
         else "mov rcx, [" ++ decorateVariableName name symbols Map.empty NO_NAMESPACE ++ "]\n" ++
              "sub rsp, 32\n" ++
              "call [" ++ bbFunctionPrefix ++ "free_string wrt ..got]\n" ++
              "add rsp, 32\n\n"
#endif

#if LINUX==1 || MAC_OS==1
arrayFree :: SymbolTable -> SymbolTable -> Symbol -> CodeTransformation ()
arrayFree symbols localSymbols (Array name (VARIABLE_TYPE_ARRAY targetType) dimensionality) =
        do if targetType == VARIABLE_TYPE_STRING
             then putAsm
                       ["mov rdi, [" ++ decorateVariableName name symbols localSymbols NO_NAMESPACE ++ "]\n",
                        "mov rsi, " ++ bbFunctionPrefix ++ "free_string\n",
                        "mov rdx, " ++ show dimensionality ++ "\n",
                        "mov rax, 0\n",
                        "call [" ++ bbFunctionPrefix ++ "deallocate_array wrt ..got]\n"]
             else putAsm
                       ["mov rdi, [" ++ decorateVariableName name symbols localSymbols NO_NAMESPACE ++ "]\n",
                        "mov rsi, 0\n",
                        "mov rdx, " ++ show dimensionality ++ "\n",
                        "mov rax, 0\n",
                        "call [" ++ bbFunctionPrefix ++ "deallocate_array wrt ..got]\n"]
#elif WINDOWS==1
arrayFree :: SymbolTable -> SymbolTable -> Symbol -> CodeTransformation ()
arrayFree symbols localSymbols (Array name (VARIABLE_TYPE_ARRAY targetType) dimensionality) =
        do if targetType == VARIABLE_TYPE_STRING
             then putAsm
                       ["mov rcx, [" ++ decorateVariableName name symbols localSymbols NO_NAMESPACE ++ "]\n",
                        "mov rdx, [" ++ bbFunctionPrefix ++ "free_string]\n",
                        "mov r8, " ++ show dimensionality ++ "\n",
                        "sub rsp, 32\n",
                        "call [" ++ bbFunctionPrefix ++ "deallocate_array wrt ..got]\n",
                        "add rsp, 32\n"]
             else putAsm
                       ["mov rcx, [" ++ decorateVariableName name symbols localSymbols NO_NAMESPACE ++ "]\n",
                        "mov rdx, 0\n",
                        "mov r8, " ++ show dimensionality ++ "\n",
                        "sub rsp, 32\n",
                        "call [" ++ bbFunctionPrefix ++ "deallocate_array]\n",
                        "add rsp, 32\n"]
#endif
                        
nextLabelID :: CodeTransformation ()
nextLabelID =
        do state <- get
           let labelID = compileStateLabelID state
           put $ state {compileStateLabelID = labelID + 1}

generateLabelName :: Int -> String
generateLabelName id =
        ".BBlabel_" ++ show id

generateIntConstantLabelName :: Int -> String
generateIntConstantLabelName id =
        "IC" ++ show id

generateFloatConstantLabelName :: Int -> String
generateFloatConstantLabelName id =
        "FC" ++ show id

generateStringConstantLabelName :: Int -> String
generateStringConstantLabelName id =
        "SC" ++ show id

decorateVariableName :: String -> SymbolTable -> SymbolTable -> Symbol -> String
decorateVariableName sourceName symbols localSymbols nameSpace =

        let name = removeTypeTag (map toLower sourceName)
            symbol = lookupVariable name symbols localSymbols nameSpace in
 
        case symbol of
             (Const {constType = VARIABLE_TYPE_INT}) -> "BBci_" ++ name
             (Const {constType = VARIABLE_TYPE_FLOAT}) -> "BBcf_" ++ name
             (Const {constType = VARIABLE_TYPE_STRING}) -> "BBcs_" ++ name
             (Variable {variableType = VARIABLE_TYPE_INT}) -> "BBi_" ++ name
             (Variable {variableType = VARIABLE_TYPE_FLOAT}) -> "BBf_" ++ name
             (Variable {variableType = VARIABLE_TYPE_STRING}) -> "BBs_" ++ name
             (Variable {variableType = VARIABLE_TYPE_CUSTOM {customTypeName = typeName}}) -> "BBc_" ++ typeName ++ "_" ++ name
             (LocalAutomaticVariable {localAutomaticVariableAddress = offset}) -> "rbp + " ++ (show offset)
             (Array {}) -> "BBa_" ++ takeWhile isAlphaUnderscore name
             k -> error ("Critical error in decorateVariableName.")
        where isAlphaUnderscore char = isAlpha (char) || char == '_'

decorateLabelName :: String -> String
decorateLabelName string = ".BBlabel_" ++ string

decorateLinkedListName :: String -> String
decorateLinkedListName string = "BB_LL_" ++ (map toLower string)

decorateTypeStringOffsetsName :: String -> String
decorateTypeStringOffsetsName string = "BB_STRINGS_" ++ (map toLower string)

isArithmeticExpressionID :: StatementID -> Bool
isArithmeticExpressionID id
        | id `elem` [EXPRESSION_ADD,
                     EXPRESSION_SUB,
                     EXPRESSION_MUL,
                     EXPRESSION_DIV,
                     EXPRESSION_POW,
                     EXPRESSION_MOD,
                     EXPRESSION_POS,
                     EXPRESSION_NEG]
                     = True
        | otherwise = False

isLogicalExpressionID :: StatementID -> Bool
isLogicalExpressionID id
        | id == EXPRESSION_NOT
                = True
        | otherwise = False

isBitwiseExpressionID :: StatementID -> Bool
isBitwiseExpressionID id
        | id `elem` [EXPRESSION_AND,
                     EXPRESSION_OR,
                     EXPRESSION_XOR,
                     EXPRESSION_BITWISE_COMPLEMENT]
                     = True
        | otherwise = False

isShiftExpressionID :: StatementID -> Bool
isShiftExpressionID id
        | id `elem` [EXPRESSION_SHL,
                     EXPRESSION_SHR,
                     EXPRESSION_SAR]
                     = True
        | otherwise = False

isRelationalExpressionID :: StatementID -> Bool
isRelationalExpressionID id
  | id `elem` relationalOperatorList = True
  | otherwise = False

isNewExpressionID :: StatementID -> Bool
isNewExpressionID id
  | id == EXPRESSION_NEW = True
  | otherwise = False

isFieldAccessExpressionID :: StatementID -> Bool
isFieldAccessExpressionID id
  | id == EXPRESSION_FIELD_ACCESS = True
  | otherwise = False

isTypeConversionExpressionID :: StatementID -> Bool
isTypeConversionExpressionID id
  | id `elem` typeConversionOperatorList = True
  | otherwise = False

isTypeListExpressionID :: StatementID -> Bool
isTypeListExpressionID id
  | id `elem` typeListOperatorList = True
  | otherwise = False

resetRegisterState :: CodeTransformation ()
resetRegisterState =
        do state <- get
           nameSpace <- gets compileStateNameSpace
           let numLocals = if nameSpace == NO_NAMESPACE
                           then 0
                           else functionNumLocals nameSpace
               offset = (numPreservedRegisters - 1) + if odd numLocals then 1 else 0
           put $ state {compileStateRegisters = createCPUContext offset}

currentRegister :: CodeState -> Register
currentRegister CompileState {compileStateRegisters = cpuContext} =
        cpuContextCurrentRegister cpuContext

setCurrentRegister :: Register -> CodeTransformation ()
setCurrentRegister register =
        do state <- get
           let pool = cpuContextPool (compileStateRegisters state)
           put $ state {compileStateRegisters = (compileStateRegisters state) {cpuContextCurrentRegister = register}}

adjustCPUContextOffset :: Int -> CodeTransformation ()
adjustCPUContextOffset adjustment =

        do state <- get
           let pool = compileStateRegisters state
               aligned = cpuContextAligned pool
               newAlignment =
                 if aligned
                 then if odd adjustment
                      then False
                      else True
                 else if odd adjustment
                      then True
                      else False

           put $ state {compileStateRegisters = (compileStateRegisters state)
                        {cpuContextOffset = cpuContextOffset pool + adjustment,
                         cpuContextAligned = newAlignment}}

flipCPUContextAlignment :: CodeTransformation ()
flipCPUContextAlignment =

        do state <- get
           let pool = compileStateRegisters state
               aligned = cpuContextAligned pool

           if aligned
           then put $ state {compileStateRegisters = (compileStateRegisters state) {cpuContextAligned = False}}
           else put $ state {compileStateRegisters = (compileStateRegisters state) {cpuContextAligned = True}}

allocateRegister :: CodeTransformation ()
allocateRegister =

  do state <- get

     let cpuContext = compileStateRegisters state
         pool = cpuContextPool cpuContext
         suggestedRegister =
           if cpuContextSuggestedRegisters cpuContext == []
           then NO_REGISTER
           else head (cpuContextSuggestedRegisters cpuContext)
         container = find (\ r -> registerAllocations r == 0 && registerReservedForFunctionCall r == False && registerExcluded r == False) pool
         
         register =
           if container == Nothing
           then error "Critical error in allocateRegister."
           else fromJust container
         allocatedRegister = Register (registerName register) (registerAllocations register + 1) False False

     if suggestedRegister /= NO_REGISTER
     then do allocateSuggestedRegister suggestedRegister
     else do if container == Nothing
             then throwCompileError "Out of general purpose registers." (compileStateLineNumber state)
             else return ()
             put $ state {compileStateRegisters = (compileStateRegisters state) {cpuContextCurrentRegister = allocatedRegister, cpuContextPool = updateRegisterPool pool allocatedRegister}}

allocateSuggestedRegister :: Register -> CodeTransformation ()
allocateSuggestedRegister desiredRegister =
  do state <- get
     let pool = cpuContextPool (compileStateRegisters state)
         container = find (\r -> registerName r == registerName desiredRegister) pool
         register =
           if container == Nothing
           then error "Critical error in allocateSuggestedRegister."
           else fromJust container
         allocatedRegister = Register (registerName register) (registerAllocations register + 1) False False

     if container == Nothing
     then do throwCompileError "Critical error in allocateSuggestedRegister." (compileStateLineNumber state)
     else do put $ state {compileStateRegisters = (compileStateRegisters state) {cpuContextCurrentRegister = allocatedRegister, cpuContextPool = updateRegisterPool pool allocatedRegister}}
             unSuggestRegister

allocateSpecificRegister :: Register -> CodeTransformation ()
allocateSpecificRegister desiredRegister =

        do state <- get

           let pool = cpuContextPool (compileStateRegisters state)
               container = find (\ r -> registerName r == registerName desiredRegister && registerExcluded r == False) pool
               register = fromJust container
               allocatedRegister = Register (registerName register) (registerAllocations register + 1) False False

           if container == Nothing
           then throwCompileError "Out of general purpose registers." (compileStateLineNumber state)
           else return ()
           put $ state {compileStateRegisters = (compileStateRegisters state) {cpuContextCurrentRegister = allocatedRegister, cpuContextPool = updateRegisterPool pool allocatedRegister}}

reserveRegisterForFunctionCallWithPreference :: Register -> CodeTransformation Register
reserveRegisterForFunctionCallWithPreference preferredRegister =

        do state <- get

           let pool = cpuContextPool (compileStateRegisters state)
               preferredRegisterContainer = find (\ r -> registerName r == registerName preferredRegister && registerAllocations r == 0 && registerReservedForFunctionCall r == False && registerExcluded r == False) pool
               suppliedRegisterContainer = find (\ r -> registerAllocations r == 0 && registerReservedForFunctionCall r == False && registerExcluded r == False) pool
               register = if preferredRegisterContainer /= Nothing
                          then fromJust preferredRegisterContainer
                          else if suppliedRegisterContainer /= Nothing
                               then fromJust suppliedRegisterContainer
                               else NO_REGISTER
               reservedRegister = Register (registerName register) (registerAllocations register) True False

           if preferredRegisterContainer == Nothing && suppliedRegisterContainer == Nothing
           then do _ <- throwCompileError "Out of general purpose registers." (compileStateLineNumber state)
                   return NO_REGISTER
           else return NO_REGISTER
           put $ state {compileStateRegisters = (compileStateRegisters state) {cpuContextCurrentRegister = reservedRegister, cpuContextPool = updateRegisterPool pool reservedRegister}}
           return reservedRegister

unreserveRegisters :: [Register] -> CodeTransformation ()
unreserveRegisters (reservedRegister : rest) =

        do state <- get
           asm <- gets compileStateAsm

           let pool = cpuContextPool (compileStateRegisters state)
               register = lookupRegister pool (registerName reservedRegister)
               deallocatedRegister = Register (registerName register) 0 False (registerExcluded register)
               updatedPool = updateRegisterPool pool deallocatedRegister
           if registerName reservedRegister == "rax"
           then do unreserveRegisters rest
           else do if not (registerReservedForFunctionCall register)
                   then throwCompileError ("Compiler attempted to unreserve a register (" ++ registerName register ++ ") not previously reserved.\n" ++ "\n" ++ show asm) (compileStateLineNumber state)
                   else return ()

                   put $ state {compileStateRegisters = (compileStateRegisters state) {cpuContextPool = updatedPool}}

                   unreserveRegisters rest

unreserveRegisters _ =

        do return ()

lookupRegister :: [Register] -> String -> Register
lookupRegister pool name =

        let container = (find (\ r -> registerName r == name) pool) in
            if container == Nothing
            then error "Critical error in function lookupRegister."
            else fromJust container

deallocateRegisters :: [Register] -> CodeTransformation ()
deallocateRegisters (allocatedRegister : rest) =

        do state <- get
           asm <- gets compileStateAsm

           let pool = cpuContextPool (compileStateRegisters state)
               indic = cpuContextSuggestedRegisters (compileStateRegisters state)
               register = lookupRegister pool (registerName allocatedRegister)
               deallocatedRegister = Register (registerName register) (registerAllocations register - 1) False (registerExcluded register)
               updatedPool = updateRegisterPool pool deallocatedRegister
           if registerName allocatedRegister == "rax"
           then do deallocateRegisters rest
           else do if not ((registerAllocations register) > 0 )
                   then throwCompileError ("Compiler attempted to deallocate a register (" ++ registerName register ++ ") not previously allocated.\n" ++ "\n" ++ show asm) (compileStateLineNumber state)
                   else return ()

                   put $ state {compileStateRegisters = (compileStateRegisters state) {cpuContextPool = updatedPool}}

                   deallocateRegisters rest

deallocateRegisters _ =

        do return ()

excludeRegister :: Register -> CodeTransformation ()
excludeRegister desiredRegister =

  do state <- get

     let pool = cpuContextPool (compileStateRegisters state)
         register = lookupRegister pool (registerName desiredRegister)
         excludedRegister = register {registerExcluded = True}

     put $ state {compileStateRegisters = (compileStateRegisters state) {cpuContextPool = updateRegisterPool pool excludedRegister}}

includeAllRegisters :: CodeTransformation ()
includeAllRegisters =

  do state <- get

     let pool = cpuContextPool (compileStateRegisters state)
         newPool = map (\r -> r {registerExcluded = False}) pool

     put $ state {compileStateRegisters = (compileStateRegisters state) {cpuContextPool = newPool}}

updateRegisterPool :: [Register] -> Register -> [Register]
updateRegisterPool pool register =

        let split = break (\ r -> registerName r == registerName register) pool in
            fst split ++
            [register] ++
            (tail . snd) split

allocateFloatRegister :: CodeTransformation ()
allocateFloatRegister =
        do state <- get
           let registers = compileStateRegisters state
           if cpuContextNumFloatRegisters registers == 0
           then throwCompileError "Out of floating point registers." (compileStateLineNumber state)
           else return ()
           let newRegisters = registers {cpuContextNumFloatRegisters = cpuContextNumFloatRegisters registers - 1}
           put $ state {compileStateRegisters = newRegisters}

deallocateFloatRegister :: CodeTransformation ()
deallocateFloatRegister =
        do state <- get
           let registers = compileStateRegisters state
           let newRegisters = registers {cpuContextNumFloatRegisters = cpuContextNumFloatRegisters registers + 1}
           put $ state {compileStateRegisters = newRegisters}

createTempVariable :: String -> CodeTransformation ()
createTempVariable name =

        do state <- get
           let variables = compileStateSymbols state
               tempVariable = Variable {variableName = (map toLower name),variableType = (readVariableType name),variableIsGlobal = True}
           put $ state {compileStateSymbols = Map.insert (map toLower (removeTypeTag name)) tempVariable variables}

isLeakyExpression :: SymbolTable -> SymbolTable -> SymbolTable -> Symbol -> Statement -> Bool
isLeakyExpression symbols localSymbols types nameSpace (Statement {statementID = STATEMENT_EXPRESSION,statementContents = (statement:_)}) =
  isLeakyExpression symbols localSymbols types nameSpace statement
  
isLeakyExpression symbols localSymbols types nameSpace expression
  | isRawRegisterStatement expression = False
  | expressionIsFunctionCall expression =
    let (name,arguments) = getFunctionCallOperands (statementContents expression)  
        f = lookupVariable (identifierExpressionValue (getInitialStatement name)) symbols localSymbols nameSpace in
             case f of
                  Function {} -> functionType f == VARIABLE_TYPE_STRING
                  _ -> False
  | statementID expression == EXPRESSION_STR = True
  | statementID expression == EXPRESSION_GROUP =
    isLeakyExpression symbols localSymbols types nameSpace (getInitialStatement expression)
  | otherwise =
    if statementID expression == EXPRESSION_ADD && any (== VARIABLE_TYPE_STRING) (map (\ e -> getExpressionType e symbols localSymbols types) (statementContents expression)) && not (expressionIsConstant symbols expression)
      then True
      else False

isLeakyArgument :: SymbolTable -> SymbolTable -> SymbolTable -> Symbol -> Statement -> Bool
isLeakyArgument symbols localSymbols types nameSpace (Statement {statementID = STATEMENT_EXPRESSION,statementContents = (statement:_)}) =
  isLeakyArgument symbols localSymbols types nameSpace statement
  
isLeakyArgument symbols localSymbols types nameSpace expression
  | isRawRegisterStatement expression = False
  | expressionIsFunctionCall expression =
    let (name,arguments) = getFunctionCallOperands (statementContents expression)  
        f = lookupVariable (identifierExpressionValue (getInitialStatement name)) symbols localSymbols nameSpace in
             case f of
                  Function {} -> functionType f == VARIABLE_TYPE_STRING
                  _ -> False
  | statementID expression == EXPRESSION_GROUP =
    isLeakyArgument symbols localSymbols types nameSpace (getInitialStatement expression)
  | statementID expression == EXPRESSION_STRING_CONSTANT = True
  | statementID expression == EXPRESSION_IDENTIFIER =
    getExpressionType expression symbols localSymbols types == VARIABLE_TYPE_STRING
  | statementID expression == EXPRESSION_ADD =
    any (== VARIABLE_TYPE_STRING) (map (\ e -> getExpressionType e symbols localSymbols types) (statementContents expression)) && not (expressionIsConstant symbols expression)
  | otherwise = error ("Critical error in isLeakyArgument.\n" ++ show expression)
                
insertArrayAccess :: String -> [Statement] -> Bool -> CodeTransformation ()
insertArrayAccess sourceName arguments dereference =

        do symbols <- gets compileStateSymbols
           localSymbols <- gets compileStateLocalSymbols
           nameSpace <- gets compileStateNameSpace
           dataType <- gets getDataType

           let name = removeTypeTag (map toLower sourceName)
               (Array _ (VARIABLE_TYPE_ARRAY arrayType) numDimensions) = lookupVariable name symbols localSymbols nameSpace
           
           anticipateFatalError (head arguments)
           
           insertFunctionCall "access_array"
             ([createMinimalStatement EXPRESSION_IDENTIFIER [IdentifierExpression name],
               createMinimalStatement EXPRESSION_INT_CONSTANT [IntConstantExpression numDimensions]] ++ arguments)
           
           if dereference
           then do case arrayType of

                        VARIABLE_TYPE_INT ->

                          do result <- gets currentRegister
                             allocateRegister
                             register <- gets currentRegister
                             putAsm
                               ["mov " ++ registerName register ++ ", [" ++ registerName result ++ "]\n"]
                             deallocateRegisters [result]

                        VARIABLE_TYPE_FLOAT ->

                          do allocateFloatRegister
                             putAsm
                               ["fld qword [rax]\n"]

                        VARIABLE_TYPE_STRING ->

                          do result <- gets currentRegister
                             allocateRegister
                             register <- gets currentRegister
                             putAsm
                               ["mov " ++ registerName register ++ ", [" ++ registerName result ++ "]\n"]
                             deallocateRegisters [result]

                        VARIABLE_TYPE_CUSTOM {} ->

                          do result <- gets currentRegister
                             allocateRegister
                             register <- gets currentRegister
                             putAsm
                               ["mov " ++ registerName register ++ ", [" ++ registerName result ++ "]\n"]
                             deallocateRegisters [result]

                   setDataType dataType
                   insertTypeFilter arrayType

           else do return ()

addFunctionCallContext :: Int -> Int -> CodeTransformation ()
addFunctionCallContext originalOffset numLeakyObjects =
  do state <- get
     cpuContext <- gets compileStateRegisters
     let functionCallContexts = cpuContextFunctionCallContexts cpuContext
         newFunctionCallContext =
           FunctionCallContext
             {
              functionCallContextFloatOffset = originalOffset + numLeakyObjects + 1,
              
              functionCallContextLeakyOffset = originalOffset + 1}
         newCPUContext =            
           cpuContext {cpuContextFunctionCallContexts = (newFunctionCallContext:functionCallContexts)}
         newState =
           state {compileStateRegisters = newCPUContext}
     put newState

removeFunctionCallContext :: CodeTransformation ()
removeFunctionCallContext =
  do state <- get
     cpuContext <- gets compileStateRegisters
     let functionCallContexts = cpuContextFunctionCallContexts cpuContext
         newCPUContext =            
           cpuContext {cpuContextFunctionCallContexts = drop 1 functionCallContexts}
         newState =
           state {compileStateRegisters = newCPUContext}
     put newState
     
incrementFunctionCallContextFloatOffset :: CodeTransformation ()
incrementFunctionCallContextFloatOffset =
  do state <- get
     cpuContext <- gets compileStateRegisters
     let functionCallContext = head (cpuContextFunctionCallContexts cpuContext)
         newFunctionCallContext =
           functionCallContext
             {functionCallContextFloatOffset = functionCallContextFloatOffset functionCallContext + 1}
         newCPUContext =
           cpuContext
             {cpuContextFunctionCallContexts = newFunctionCallContext:(drop 1 (cpuContextFunctionCallContexts cpuContext))}
         newState =
           state
             {compileStateRegisters = newCPUContext}
     put newState

incrementFunctionCallContextLeakyOffset :: CodeTransformation ()
incrementFunctionCallContextLeakyOffset =
  do state <- get
     cpuContext <- gets compileStateRegisters
     let functionCallContext = head (cpuContextFunctionCallContexts cpuContext)
         newFunctionCallContext =
           functionCallContext
             {functionCallContextLeakyOffset = functionCallContextLeakyOffset functionCallContext + 1}
         newCPUContext =
           cpuContext
             {cpuContextFunctionCallContexts = newFunctionCallContext:(drop 1 (cpuContextFunctionCallContexts cpuContext))}
         newState =
           state
             {compileStateRegisters = newCPUContext}
     put newState

isLeakyTypeConversion :: (VariableType,VariableType) -> Bool
isLeakyTypeConversion (source,dest) =
  (dest == VARIABLE_TYPE_STRING && source /= VARIABLE_TYPE_STRING)

getNumLeakyTypeConversions :: [Statement] -> [Parameter] -> SymbolTable -> SymbolTable -> SymbolTable -> Int
getNumLeakyTypeConversions arguments parameters symbols localSymbols types =
  length (filter (==True) (map isLeakyTypeConversion (zip (map (getExpressionTypeFlipped symbols localSymbols types) arguments) (map parameterType parameters))))
  where getExpressionTypeFlipped b c d a = getExpressionType a b c d

#if LINUX==1 || MAC_OS==1

insertFunctionCall :: String -> [Statement] -> CodeTransformation ()
insertFunctionCall sourceName arguments =

  do symbols <- gets compileStateSymbols
     localSymbols <- gets compileStateLocalSymbols
     types <- gets compileStateTypes
     nameSpace <- gets compileStateNameSpace
     cpuContext <- gets compileStateRegisters
          
     let name = removeTypeTag (map toLower sourceName)
         function = lookupVariable name symbols localSymbols nameSpace
         numLocals =
           if nameSpace == NO_NAMESPACE 
           then 0
           else functionNumLocals nameSpace
         pool = cpuContextPool cpuContext
         numFPRsSaved = numFPRs - cpuContextNumFloatRegisters cpuContext
         parameters = functionParameters function

         registerArguments = extractRegisterArguments (zip parameters arguments) 0 0 0
         gprArguments = filter (\a -> (not . isRawRegisterStatement) (snd a) && parameterType (fst a) /= VARIABLE_TYPE_FLOAT) registerArguments
         rawRegisterArguments = filter (\a -> isRawRegisterStatement (snd a)) registerArguments
         nonRawRegisterArguments = filter (\a -> (not . isRawRegisterStatement) (snd a)) registerArguments
         rawRegisters = map (lookupRegister pool) (map rawRegisterName (map snd rawRegisterArguments))
         nonRawRegisters = deleteFirstsBy compareRegisterNames functionCallRegisters rawRegisters

         compareRegisterNames r1 r2 = registerName r1 == registerName r2
         sourceRawRegisterNames = map (rawRegisterName . snd) rawRegisterArguments
         indicesRaw = findIndices (\a -> isRawRegisterStatement (snd a)) registerArguments
         functionCallRegistersRaw = map (functionCallRegisters !!) indicesRaw
         indicesNonRaw = findIndices (\a -> (not . isRawRegisterStatement) (snd a) && parameterType (fst a) /= VARIABLE_TYPE_FLOAT) (filter (\a -> parameterType (fst a) /= VARIABLE_TYPE_FLOAT) registerArguments)
         functionCallRegistersNonRaw = map (functionCallRegisters !!) indicesNonRaw
         destRawRegisterNames = map registerName functionCallRegistersRaw
         
         stackArguments = reverse $ extractStackArguments (zip parameters arguments) 0 0 0
         numStackArguments = length stackArguments
         numFloatArguments = length (filter (\p -> parameterType p == VARIABLE_TYPE_FLOAT) parameters)
         numMMRsUsed = length (filter (\ a -> (parameterType (fst a)) == VARIABLE_TYPE_FLOAT) registerArguments)
         numGPRsUsed = length (filter (\ a -> (parameterType (fst a)) /= VARIABLE_TYPE_FLOAT) registerArguments)
         numGPRsSaved = length $ extractAllocatedScratchRegisters pool
         numLeakyRegisterArguments = length $ filter (isLeakyExpression symbols localSymbols types nameSpace) (snd (unzip registerArguments))
         numLeakyStackArguments = length $ filter (isLeakyExpression symbols localSymbols types nameSpace) (snd (unzip stackArguments))
         numLeakyTypeConversions = getNumLeakyTypeConversions arguments parameters symbols localSymbols types
         numLeakyItems = numLeakyRegisterArguments + numLeakyStackArguments + numLeakyTypeConversions
         numAuxItems = numMMRsUsed + numLeakyItems
         numTempItems = numStackArguments + numFPRsSaved + numGPRsSaved

         aligned = cpuContextAligned cpuContext

         compensation =
           if aligned
           then if odd (numAuxItems + numTempItems)
                then 1
                else 0
           else if odd (numAuxItems + numTempItems)
                then 0
                else 1

     addFunctionCallContext (cpuContextOffset cpuContext + numLocals + compensation) numLeakyItems
     adjustCPUContextOffset (numAuxItems + compensation)
     putAsm
       ["sub rsp, " ++ show (8 * (numAuxItems + compensation)) ++ "\n"]
     insertScratchRegisterPush
     insertStackArgumentPass stackArguments
     insertRawRegisterArgumentPasses sourceRawRegisterNames destRawRegisterNames
     insertRegisterArgumentPass (reverse nonRawRegisterArguments) (reverse functionCallRegistersNonRaw)
     mapM_ (insertMMRPass multimediaRegisterNames (cpuContextOffset cpuContext + numLocals + compensation + numLeakyItems) numMMRsUsed) [0 .. numMMRsUsed - 1]
     
     if functionOrigin function == FUNCTION_ORIGIN_USER
     then do putAsm
               ["mov rax, " ++ show (min numFloatArguments numFunctionCallMMRs) ++ "\n",
                "call " ++ functionName function ++ "\n"]
     else do putAsm
               ["mov rax, " ++ show (min numFloatArguments numFunctionCallMMRs) ++ "\n",
                "call [" ++ functionName function ++ " wrt ..got]\n"]

     if functionType function == VARIABLE_TYPE_FLOAT
     then do putAsm
               ["movq rax, xmm0\n",
                "emms\n"]
     else do return ()
     putRemoveStackArguments numStackArguments
     deallocateRegisters functionCallRegistersNonRaw
     
     insertPushLinuxScratch numLeakyItems
     
     mapM_ (insertLeakyArgumentFree (cpuContextOffset cpuContext + numLocals + compensation)) [1 .. numLeakyItems]
     insertPopLinuxScratch numLeakyItems
     insertScratchRegisterPop
     putAsm
       ["add rsp, " ++ show (8 * (numAuxItems + compensation)) ++ "\n"]
     adjustCPUContextOffset (-(numAuxItems + compensation))
     removeFunctionCallContext
     allocateRegister
     result <- gets currentRegister
     putAsm
       ["mov " ++ registerName result ++ ", rax\n"]

     where putRemoveStackArguments numStackArguments =
             do if numStackArguments > 0
                then do putAsm
                          ["add rsp, " ++ show (8 * numStackArguments) ++ "\n"]
                        adjustCPUContextOffset (-numStackArguments)
                else return ()
           insertMMRPass mmrs adjustment numMMRsUsed offset =
             do putAsm
                  ["movq " ++ (mmrs !! offset) ++ ", [rbp - " ++ show (8 * (adjustment + (numMMRsUsed - offset))) ++ "]\n"]
           insertPushLinuxScratch numLeakyItems =
             do if numLeakyItems > 0
                then putAsm
                       ["push rax\n",
                        "push rdi\n"]
                else return ()
           insertPopLinuxScratch numLeakyItems =
             do if numLeakyItems > 0
                then putAsm
                       ["pop rdi\n",
                        "pop rax\n"]
                else return ()
           insertLeakyArgumentFree adjustment offset =
             do putAsm
                  ["mov rax, 0\n",
                   "mov rdi, [rbp - " ++ show (8 * (adjustment + offset)) ++ "]\n",
                   "call [" ++ bbFunctionPrefix ++ "free_string wrt ..got]\n"]
#elif WINDOWS == 1
insertFunctionCall :: String -> [Statement] -> CodeTransformation ()
insertFunctionCall sourceName arguments =

  do symbols <- gets compileStateSymbols
     localSymbols <- gets compileStateLocalSymbols
     types <- gets compileStateTypes
     nameSpace <- gets compileStateNameSpace
     cpuContext <- gets compileStateRegisters
          
     let name = removeTypeTag (map toLower sourceName)
         function = lookupVariable name symbols localSymbols nameSpace
         numLocals =
           if nameSpace == NO_NAMESPACE 
           then 0
           else functionNumLocals nameSpace
         pool = cpuContextPool cpuContext
         numFPRsSaved = numFPRs - cpuContextNumFloatRegisters cpuContext
         parameters = functionParameters function

         registerArguments = extractRegisterArguments (zip parameters arguments)
         rawRegisterArguments = filter (\a -> isRawRegisterStatement (snd a)) registerArguments
         nonRawRegisterArguments = filter (\a -> (not . isRawRegisterStatement) (snd a)) registerArguments
         rawRegisters = map (lookupRegister pool) (map rawRegisterName (map snd rawRegisterArguments))
         nonRawRegisters = deleteFirstsBy compareRegisterNames functionCallRegisters rawRegisters

         compareRegisterNames r1 r2 = registerName r1 == registerName r2
         sourceRawRegisterNames = map (rawRegisterName . snd) rawRegisterArguments
         indicesRaw = findIndices (\a -> isRawRegisterStatement (snd a)) registerArguments
         functionCallRegistersRaw = map (functionCallRegisters !!) indicesRaw
         indicesNonRaw = findIndices (\a -> (not . isRawRegisterStatement) (snd a) && parameterType (fst a) /= VARIABLE_TYPE_FLOAT) registerArguments
         functionCallRegistersNonRaw = map (functionCallRegisters !!) indicesNonRaw
         destRawRegisterNames = map registerName functionCallRegistersRaw
 
         stackArguments = reverse $ extractStackArguments (zip parameters arguments)
         numStackArguments = length stackArguments
         numFloatArguments = length (filter (\p -> parameterType p == VARIABLE_TYPE_FLOAT) (take 4 parameters))
         numMMRsUsed = length (filter (\ a -> (parameterType (fst a)) == VARIABLE_TYPE_FLOAT) registerArguments)
         numGPRsUsed = length (filter (\ a -> (parameterType (fst a)) /= VARIABLE_TYPE_FLOAT) registerArguments)
         numGPRsSaved = length $ extractAllocatedScratchRegisters pool
         numLeakyRegisterArguments = length $ filter (isLeakyExpression symbols localSymbols types nameSpace) (snd (unzip registerArguments))
         numLeakyStackArguments = length $ filter (isLeakyExpression symbols localSymbols types nameSpace) (snd (unzip stackArguments))
         numLeakyTypeConversions = getNumLeakyTypeConversions arguments parameters symbols localSymbols types
         numLeakyItems = numLeakyRegisterArguments + numLeakyStackArguments + numLeakyTypeConversions
         numAuxItems = numMMRsUsed + numLeakyItems
         numTempItems = numStackArguments + numFPRsSaved + numGPRsSaved
         
         registerArgumentTypes = map (parameterType . fst) (reverse registerArguments)
         functionCallRegistersWin = (snd . unzip) (filter (\ a -> fst a `notElem` [VARIABLE_TYPE_FLOAT]) (zip registerArgumentTypes functionCallRegisters))
         
         indicesMMR = findIndices (\a -> (not . isRawRegisterStatement) (snd a) && parameterType (fst a) == VARIABLE_TYPE_FLOAT) registerArguments
         multimediaRegisterNamesWin = take 4 (map (multimediaRegisterNames !!) indicesMMR)
         
         aligned = cpuContextAligned cpuContext

         compensation =
           if aligned
           then if odd (numAuxItems + numTempItems)
                then 1
                else 0
           else if odd (numAuxItems + numTempItems)
                then 0
                else 1
                     
     addFunctionCallContext (cpuContextOffset cpuContext + numLocals + compensation) numLeakyItems
     adjustCPUContextOffset (numAuxItems + compensation)
     putAsm
       ["sub rsp, " ++ show (8 * (numAuxItems + compensation)) ++ "\n"]
     insertScratchRegisterPush
     insertStackArgumentPass stackArguments
     insertRawRegisterArgumentPasses sourceRawRegisterNames destRawRegisterNames
     insertRegisterArgumentPass (reverse nonRawRegisterArguments) (reverse functionCallRegistersNonRaw)

     mapM_ (insertMMRPass multimediaRegisterNamesWin (cpuContextOffset cpuContext + numLocals + compensation + numLeakyItems) numMMRsUsed) [0 .. numMMRsUsed - 1]
     putAsm
       ["mov rax, " ++ show (min numFloatArguments numFunctionCallMMRs) ++ "\n",
        "sub rsp, 32\n",
        "call [" ++ functionName function ++ " wrt ..got]\n",
        "add rsp, 32\n"]
     if functionType function == VARIABLE_TYPE_FLOAT
     then do putAsm
               ["movq rax, xmm0\n",
                "emms\n"]
     else do return ()
     putRemoveStackArguments numStackArguments
     deallocateRegisters functionCallRegistersNonRaw
     insertPushWindowsScratch numLeakyItems
     mapM_ (insertLeakyArgumentFree (cpuContextOffset cpuContext + numLocals + compensation)) [1 .. numLeakyItems]
     insertPopWindowsScratch numLeakyItems
     insertScratchRegisterPop
     putAsm
       ["add rsp, " ++ show (8 * (numAuxItems + compensation)) ++ "\n"]
     adjustCPUContextOffset (-(numAuxItems + compensation))
     removeFunctionCallContext
     allocateRegister
     result <- gets currentRegister
     putAsm
       ["mov " ++ registerName result ++ ", rax\n"]

     where putRemoveStackArguments numStackArguments =
             do if numStackArguments > 0
                then do putAsm
                          ["add rsp, " ++ show (8 * numStackArguments) ++ "\n"]
                        adjustCPUContextOffset (-numStackArguments)
                else return ()
           insertMMRPass mmrs adjustment numMMRsUsed offset =
             do if offset < length mmrs
                then putAsm
                       ["movq " ++ (mmrs !! offset) ++ ", [rbp - " ++ show (8 * (adjustment + (numMMRsUsed - offset))) ++ "]\n"]
                else return ()       
           insertPushWindowsScratch numLeakyItems =
             do if numLeakyItems > 0
                then putAsm
                       ["push rax\n",
                        "push rcx\n",
                        "sub rsp, 32\n"]
                else return ()
           insertPopWindowsScratch numLeakyItems =
             do if numLeakyItems > 0
                then putAsm
                       ["add rsp, 32\n",
                        "pop rcx\n",
                        "pop rax\n"]
                else return ()
           insertLeakyArgumentFree adjustment offset =
             do putAsm
                  ["mov rax, 0\n",
                   "mov rcx, [rbp - " ++ show (8 * (adjustment + offset)) ++ "]\n",
                   "call [" ++ bbFunctionPrefix ++ "free_string wrt ..got]\n"]
#endif

insertRawRegisterArgumentPasses :: [String] -> [String] -> CodeTransformation ()
insertRawRegisterArgumentPasses statementRegisterNames gprNames =

  do state <- get
     let pool = cpuContextPool (compileStateRegisters state)
         namePairs = zip gprNames statementRegisterNames

     putPass namePairs
     unreserveRegisters (map (lookupRegister pool) statementRegisterNames)
     
  where mirrors (destName1,sourceName1) (destName2,sourceName2) =
          if destName1 == sourceName2
          then (destName2,sourceName2)
          else ("","")
        
        putPass ((destName,sourceName):rest) =
          do let findMirrors = (map (mirrors (destName,sourceName)) rest)
                 mirror = if findMirrors == []
                          then ("","")
                          else head findMirrors
             if mirror == ("","")
             then do if destName /= sourceName
                     then do putAsm
                               ["mov " ++ destName ++ ", " ++ sourceName ++ "\n"]
                     else return ()
                     putPass rest
             else do putAsm
                       ["push " ++ destName ++ "\n",
                        "mov " ++ destName ++ ", " ++ sourceName ++ "\n",
                        "pop " ++ sourceName ++ "\n"]
                     let newRest = (takeWhile (\p -> p /= mirror) rest) ++ (tail (dropWhile (\p -> p /= mirror) rest))
                     putPass newRest
        putPass _ = return ()
        
suggestRegister :: Register -> CodeTransformation ()
suggestRegister register =
  do state <- get
     let pool = compileStateRegisters state
         newPool = pool {cpuContextSuggestedRegisters = register:(cpuContextSuggestedRegisters pool)}
     put state {compileStateRegisters = newPool}

unSuggestRegister :: CodeTransformation ()
unSuggestRegister =
  do state <- get
     let pool = compileStateRegisters state
         newPool = pool {cpuContextSuggestedRegisters = NO_REGISTER:(drop 1 (cpuContextSuggestedRegisters pool))}
     put state {compileStateRegisters = newPool}

insertRegisterArgumentPass :: [(Parameter, Statement)] -> [Register] -> CodeTransformation ()
insertRegisterArgumentPass ((Parameter {parameterType = destType}, expression) : rest) gprs

  | destType `elem` [VARIABLE_TYPE_INT, VARIABLE_TYPE_STRING] || isCustomType destType =

    do symbols <- gets compileStateSymbols
       localSymbols <- gets compileStateLocalSymbols
       types <- gets compileStateTypes
       nameSpace <- gets compileStateNameSpace
       cpuContext <- gets compileStateRegisters

       suggestRegister (head gprs)
               
       setDataType destType
               
       compileExpression expression

       result <- gets currentRegister
               
       if isLeakyExpression symbols localSymbols types nameSpace expression || isLeakyTypeConversion (getExpressionType expression symbols localSymbols types,destType)
       then do leakyOffset <- gets (functionCallContextLeakyOffset . head . cpuContextFunctionCallContexts . compileStateRegisters)
               putAsm
                 ["mov [rbp - " ++ show (8 * leakyOffset) ++ "], " ++ registerName result ++ "\n"]
               incrementFunctionCallContextLeakyOffset
       else return ()
               
       if registerName result /= registerName (head gprs)
       then do putAsm
                 ["mov " ++ registerName (head gprs) ++ ", " ++ registerName result ++ "\n"]
               deallocateRegisters [result]
               allocateSpecificRegister (head gprs)
       else return ()

       insertRegisterArgumentPass rest (drop 1 gprs)

  | destType == VARIABLE_TYPE_FLOAT =

    do symbols <- gets compileStateSymbols
       localSymbols <- gets compileStateLocalSymbols
       types <- gets compileStateTypes
       nameSpace <- gets compileStateNameSpace
       cpuContext <- gets compileStateRegisters

       setDataType (getExpressionType expression symbols localSymbols types)
       compileExpression expression
       result <- gets currentRegister

       if isLeakyExpression symbols localSymbols types nameSpace expression
       then do leakyOffset <- gets (functionCallContextLeakyOffset . head . cpuContextFunctionCallContexts . compileStateRegisters)
               putAsm
                 ["mov [rbp - " ++ show (8 * leakyOffset) ++ "], " ++ registerName result ++ "\n"]
               incrementFunctionCallContextLeakyOffset
       else return ()

       setDataType destType
       insertTypeFilter (getExpressionType expression symbols localSymbols types)

       floatOffset <- gets (functionCallContextFloatOffset . head . cpuContextFunctionCallContexts . compileStateRegisters)

       putAsm
         ["fstp qword [rbp - " ++ show (8 * floatOffset) ++ "]\n",
          "fwait\n"]
       
       incrementFunctionCallContextFloatOffset
       deallocateFloatRegister

       insertRegisterArgumentPass rest gprs

insertRegisterArgumentPass _ _ =

        do return ()

insertStackArgumentPass :: [(Parameter, Statement)] -> CodeTransformation ()
insertStackArgumentPass ((Parameter {parameterType = destType}, expression) : rest)

  | destType `elem` [VARIABLE_TYPE_INT, VARIABLE_TYPE_STRING] || isCustomType destType =

    do symbols <- gets compileStateSymbols
       localSymbols <- gets compileStateLocalSymbols
       types <- gets compileStateTypes
       nameSpace <- gets compileStateNameSpace
       cpuContext <- gets compileStateRegisters

       setDataType destType
       compileExpression expression
       result <- gets currentRegister

       if isLeakyExpression symbols localSymbols types nameSpace expression || isLeakyTypeConversion (getExpressionType expression symbols localSymbols types,destType)
       then do leakyOffset <- gets (functionCallContextLeakyOffset . head . cpuContextFunctionCallContexts . compileStateRegisters)
               putAsm
                 ["mov [rbp - " ++ show (8 * leakyOffset) ++ "], " ++ registerName result ++ "\n"]
               incrementFunctionCallContextLeakyOffset
       else return ()

       putAsm
         ["push " ++ registerName result ++ "\n"]

       adjustCPUContextOffset 1

       deallocateRegisters [result]
       
       insertStackArgumentPass rest

  | destType == VARIABLE_TYPE_FLOAT =

    do symbols <- gets compileStateSymbols
       localSymbols <- gets compileStateLocalSymbols
       types <- gets compileStateTypes
       nameSpace <- gets compileStateNameSpace
       cpuContext <- gets compileStateRegisters
       
       setDataType (getExpressionType expression symbols localSymbols types)
       compileExpression expression
       result <- gets currentRegister

       if isLeakyExpression symbols localSymbols types nameSpace expression
       then do leakyOffset <- gets (functionCallContextLeakyOffset . head . cpuContextFunctionCallContexts . compileStateRegisters)
               putAsm
                 ["mov [rbp - " ++ show (8 * leakyOffset) ++ "], " ++ registerName result ++ "\n"]
               incrementFunctionCallContextLeakyOffset
       else return ()

       setDataType destType
       insertTypeFilter (getExpressionType expression symbols localSymbols types)

       putAsm
         ["sub rsp, 8\n",
          "fstp qword [rsp]\n",
          "fwait\n"]

       adjustCPUContextOffset 1
       deallocateFloatRegister
       insertStackArgumentPass rest

insertStackArgumentPass _ =
  do return ()

extractAllocatedScratchRegisters :: [Register] -> [Register]
extractAllocatedScratchRegisters pool =

  deleteFirstsBy equalityTest scratchRegisters (deleteFirstsBy equalityTest (filter (\ r -> registerAllocations r == 0) pool) preservedRegisters)
  
  where equalityTest a b = registerName a == registerName b

insertScratchRegisterPush :: CodeTransformation ()
insertScratchRegisterPush =
  
        do state <- get

           let CPUContext {cpuContextNumFloatRegisters = numFloats,
                                cpuContextPool = pool} = compileStateRegisters state
               registerList = extractAllocatedScratchRegisters pool
               numFloatsUsed = numFPRs - numFloats
               pushGPRs = if registerList == []
                          then [""]
                          else (map generatePush (map registerName registerList))
               pushFPRs = concat $ take numFloatsUsed (repeat ["sub rsp, 8\n", "fstp qword [rsp]\n"])
               alignStack = ["sub rsp, 8\n"]

           adjustCPUContextOffset (length registerList + numFloatsUsed)
           putAsm
             (if length registerList > 0 || numFloatsUsed > 0
              then pushGPRs ++ pushFPRs
              else [""])
             
           where generatePush string = "push " ++ string ++ "\n"

insertScratchRegisterPop :: CodeTransformation ()
insertScratchRegisterPop =

        do state <- get
           let CPUContext {cpuContextNumFloatRegisters = numFloats,
                                cpuContextPool = pool} = compileStateRegisters state
               registerList = reverse (extractAllocatedScratchRegisters pool)
               numFloatsUsed = numFPRs - numFloats
               popGPRs = if registerList == []
                         then [""]
                         else map generatePop (map registerName registerList)
               popFPRs = concat $ take numFloatsUsed (repeat ["fld qword [rsp]\n", "add rsp, 8\n"])
               alignStack = ["add rsp, 8\n"]

           adjustCPUContextOffset (-(length registerList + numFloatsUsed))
           putAsm
             (if length registerList > 0 || numFloatsUsed > 0
              then popFPRs ++ popGPRs
              else [""])
           
           where generatePop string = "pop " ++ string ++ "\n"

setAsmBucket :: String -> CodeTransformation ()
setAsmBucket name =

        do state <- get
           let (Asm bucket bucketMap) = compileStateAsm state 
           put state {compileStateAsm = Asm name bucketMap}

getAsmBucket :: CodeTransformation String
getAsmBucket =

        do state <- get
           let (Asm bucket _) = compileStateAsm state
           return bucket

setLocalSymbols :: SymbolTable -> CodeTransformation ()
setLocalSymbols symbolTable =

        do state <- get
           put state {compileStateLocalSymbols = symbolTable}

getLocalSymbols :: CodeTransformation SymbolTable
getLocalSymbols =

        do localSymbols <- gets compileStateLocalSymbols
           return localSymbols


setDataType :: VariableType -> CodeTransformation ()
setDataType dataType =
        do state <- get
           put $ state {compileStateRegisters = (compileStateRegisters state) {cpuContextDataType = dataType}}

getDataType :: CodeState -> VariableType
getDataType CompileState {compileStateRegisters = CPUContext {cpuContextDataType = dataType}} =
        dataType


compilerIncrementLineNumber :: CodeTransformation ()
compilerIncrementLineNumber =
        do state <- get
           put state {compileStateLineNumber = (compileStateLineNumber state) + 1}

throwCompileError :: String -> Int -> CodeTransformation ()
throwCompileError msg lineNumber  =
  do state <- get
     let fileName = (head . compileStateIncludeFileNameStack) state
     throwError (FatalError fileName lineNumber 0 msg)
