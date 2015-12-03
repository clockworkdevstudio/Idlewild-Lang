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

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP #-}

module Semantics where

import ParserData
import SemanticsData
import CompilerData

import Common
import Options
import Lexer
import Parser

import Data.Char
import Data.List
import Data.Ord
import Data.Bits
import Data.Fixed
import Data.Maybe
import Debug.Trace

import System.IO
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Identity

import qualified Data.Sequence as Seq
import qualified Data.Map as Map
  
semantics :: CodeTransformation ()
semantics =
  do verbose <- gets (optionVerbose . configOptions . semanticStateConfig)
     console <- gets (optionConsole . configOptions . semanticStateConfig)
     (Statement {statementContents = (program:_)}:_)  <-  gets semanticStateProgram
     addEndFunction
     verboseCommentary ("Populating symbol tables...\n") verbose
     collectSymbols program
     verboseCommentary ("Checking program semantics...\n") verbose
     codeSemantics program
     
     markStandardFunctionAsUsed "init_libkoshka_core"
     markStandardFunctionAsUsed "final_libkoshka_core"
     
     if not console
     then do markStandardFunctionAsUsed "init_libkoshka_mm"
             markStandardFunctionAsUsed "final_libkoshka_mm"
     else return ()
     
     markStandardFunctionAsUsed "fatal_error"
     
     markStandardFunctionAsUsed "create_linked_list"
     markStandardFunctionAsUsed "destroy_linked_list"
     markStandardFunctionAsUsed "convert_int_to_string"
     markStandardFunctionAsUsed "convert_pointer_to_string"
     markStandardFunctionAsUsed "convert_float_to_string"
     markStandardFunctionAsUsed "convert_string_to_int"
     markStandardFunctionAsUsed "convert_string_to_float"
     markStandardFunctionAsUsed "pow"
     markStandardFunctionAsUsed "fmod"
     
     markStandardFunctionAsUsed "allocate_array"
     markStandardFunctionAsUsed "access_array"
     markStandardFunctionAsUsed "deallocate_array"

     markStandardFunctionAsUsed "init_gosub"
     markStandardFunctionAsUsed "final_gosub"     
     
     markStandardFunctionAsUsed "create_linked_list"
     markStandardFunctionAsUsed "destroy_linked_list"
     markStandardFunctionAsUsed "new"
     markStandardFunctionAsUsed "first"
     markStandardFunctionAsUsed "last"
     markStandardFunctionAsUsed "before"
     markStandardFunctionAsUsed "after"
     markStandardFunctionAsUsed "insert_before"
     markStandardFunctionAsUsed "insert_after"
     markStandardFunctionAsUsed "init_for_each"
     markStandardFunctionAsUsed "next_for_each"
     markStandardFunctionAsUsed "final_for_each"
     markStandardFunctionAsUsed "delete"
     markStandardFunctionAsUsed "delete_each"

     markStandardFunctionAsUsed "read"
     markStandardFunctionAsUsed "restore"

     markStandardFunctionAsUsed "copy_string"
     markStandardFunctionAsUsed "duplicate_string"
     markStandardFunctionAsUsed "concatenate_strings"
     markStandardFunctionAsUsed "compare_strings"
     markStandardFunctionAsUsed "free_string"
     markStandardFunctionAsUsed "free_strings"

     includeFileNameStack <- gets semanticStateIncludeFileNameStack
     program <- gets semanticStateProgram
     symbols <- gets semanticStateSymbols
     ints <- gets semanticStateInts
     floats <- gets semanticStateFloats
     strings <- gets semanticStateStrings
     config <- gets semanticStateConfig
     types <- gets semanticStateTypes
     let options = configOptions config

     put CompileState
           {compileStateIncludeFileNameStack = includeFileNameStack,
            compileStateProgram = program,
            compileStateAsm = createAsm,
            compileStateSymbols = symbols,
            compileStateLocalSymbols = Map.empty,
            compileStateTypes = types,
            compileStateInts = ints,
            compileStateFloats = floats,
            compileStateStrings = strings,
            compileStateNameSpace = NO_NAMESPACE,
            compileStateExitLabelIDs = [],
            compileStateRegisters = createCPUContext (numPreservedRegisters - 1),
            compileStateLineNumber = 1,
            compileStateLabelID = 0,
            compileStateConfig = config
           }
     
addEndFunction :: CodeTransformation ()
addEndFunction =
  do let end = createUserFunction "end" VARIABLE_TYPE_INT [(Parameter "exit_code" VARIABLE_TYPE_INT (createMinimalStatement EXPRESSION_INT_CONSTANT [IntConstantExpression 0]))] 1 0 1 0 0 Map.empty
     addFunction end "end"

semanticsPushIncludeFileName :: String -> CodeTransformation ()
semanticsPushIncludeFileName fileName =
  do state <- get
     put $ state {semanticStateIncludeFileNameStack =
                  (fileName:semanticStateIncludeFileNameStack state)}

semanticsPopIncludeFileName :: CodeTransformation ()
semanticsPopIncludeFileName =
  do state <- get
     put $ state {semanticStateIncludeFileNameStack =
                  tail (semanticStateIncludeFileNameStack state)}

codeSemantics :: Statement -> CodeTransformation ()
codeSemantics (Statement {statementContents = statements})

        | statements == [] =
            do return ()

        | otherwise =

            do semanticFunc statement
               codeSemantics (createMinimalStatement STATEMENT_CODE (tail statements))

        where statement = head statements
              semanticFunc = case statementID statement of
                                  STATEMENT_BEGINNING_OF_FILE -> beginningOfFileSemantics
                                  STATEMENT_END_OF_FILE -> endOfFileSemantics
                                  STATEMENT_CODE -> codeSemantics
                                  STATEMENT_INSERT_BEFORE -> insertSemantics
                                  STATEMENT_INSERT_AFTER -> insertSemantics
                                  STATEMENT_DELETE -> deleteSemantics
                                  STATEMENT_DELETE_EACH -> deleteEachSemantics
                                  STATEMENT_DIM -> dimStatementSemantics
                                  STATEMENT_EXPRESSION -> expressionSemantics
                                  STATEMENT_DATA -> dataSemantics
                                  STATEMENT_READ -> readSemantics
                                  STATEMENT_RESTORE -> restoreSemantics
                                  STATEMENT_FOR -> forSemantics
                                  STATEMENT_FOR_EACH -> forEachSemantics
                                  STATEMENT_WHILE -> whileSemantics
                                  STATEMENT_REPEAT -> repeatSemantics
                                  STATEMENT_EXIT -> exitSemantics
                                  STATEMENT_IF -> ifSemantics
                                  STATEMENT_SELECT -> selectSemantics
                                  STATEMENT_LABEL -> labelSemantics
                                  STATEMENT_DATA_LABEL -> dataLabelSemantics
                                  STATEMENT_ON_GOTO -> onSemantics
                                  STATEMENT_ON_GOSUB -> onSemantics
                                  STATEMENT_GOTO -> gotoSemantics
                                  STATEMENT_GOSUB -> gosubSemantics
                                  STATEMENT_MULTI_FUNCTION -> functionSemantics
                                  STATEMENT_MULTI_FUNCTION_RETURN -> functionReturnSemantics
                                  STATEMENT_GLOBAL -> globalSemantics
                                  STATEMENT_LOCAL -> localSemantics
                                  _ -> doNothing
              doNothing _ = do return ()

beginningOfFileSemantics :: Statement -> CodeTransformation ()
beginningOfFileSemantics (Statement {statementContents = (IdentifierExpression fileName:_)}) =
  do semanticsPushIncludeFileName fileName

endOfFileSemantics :: Statement -> CodeTransformation ()
endOfFileSemantics _ =
  do semanticsPopIncludeFileName

collectSymbols :: Statement -> CodeTransformation ()
collectSymbols (Statement {statementContents = statements})

  | statements == [] = do return ()
  | otherwise =

    do case statement of
         (Statement {statementContents = contents}) ->
           do let descend = mapM_ collectSymbols contents in
         
               case statementID statement of
                 STATEMENT_IF -> descend
                 STATEMENT_IF_DEFAULT -> descend
                 STATEMENT_ELSE -> descend
                 STATEMENT_ELSE_IF -> descend
                 STATEMENT_SELECT -> descend
                 STATEMENT_CASE -> descend
                 STATEMENT_DEFAULT -> descend
                 STATEMENT_FOR -> descend
                 STATEMENT_FOR_EACH -> descend
                 STATEMENT_REPEAT -> descend
                 STATEMENT_WHILE -> descend
                 STATEMENT_CODE -> descend
                 _ -> semanticFunc statement

         _ -> return ()
      
       collectSymbols (createMinimalStatement STATEMENT_CODE (tail statements))

  where statement = head statements
        semanticFunc = case statementID statement of
          STATEMENT_TYPE -> collectTypeSymbols
          STATEMENT_SYS -> collectSysSymbols
          STATEMENT_MULTI_FUNCTION -> collectFunctionSymbols
          STATEMENT_CONST -> collectConstSymbols
          STATEMENT_DIM -> collectDimStatementSymbols
          STATEMENT_LABEL -> collectLabelSymbol
          STATEMENT_DATA_LABEL -> collectLabelSymbol
          
          _ -> doNothing

        doNothing _ = do return ()

dimStatementSemantics :: Statement -> CodeTransformation ()
dimStatementSemantics (Statement {statementContents = expressionList}) =
        
        do mapM_ dimSemantics expressionList

dimSemantics :: Statement -> CodeTransformation ()
dimSemantics arrayDeclaration = 

  do symbols <- gets semanticStateSymbols
     let (identifier,dimensions) = getFunctionCallOperands (statementContents (getInitialStatement arrayDeclaration))
         sourceName = identifierExpressionValue (getInitialStatement identifier)
         name = removeTypeTag (map toLower sourceName)
         symbolContainer = Map.lookup name symbols
         symbol = fromJust symbolContainer
         
     case symbol of

       (Variable {variableType = dataType}) ->

         throwSemanticError ("Duplicate identifier '" ++ sourceName ++ "'.") arrayDeclaration

       (Array {arrayName = name, arrayType = dataType, arrayNumDimensions = numDimensions}) ->

         if length dimensions /= numDimensions
         then throwSemanticError ("Array '" ++ name ++ "': originally declared as having " ++ (show numDimensions) ++ " dimension(s); now referenced as having " ++ (show (length dimensions)) ++ " dimension(s).") arrayDeclaration
         else return ()

collectSysSymbols :: Statement -> CodeTransformation ()
collectSysSymbols (Statement {statementContents = statements}) =
  
  do symbols <- gets semanticStateSymbols
     localSymbols <- gets semanticStateLocalSymbols

     let (Statement {statementContents = (identifier:_)}:prototype:_) = statements
         sourceName = identifierExpressionValue (getInitialStatement identifier)
         parameters = statementContents prototype
         requiredParameters = takeWhile (\p -> statementID (getInitialStatement p) == EXPRESSION_IDENTIFIER) parameters
         optionalParameters = dropWhile (\p -> statementID (getInitialStatement p) == EXPRESSION_IDENTIFIER) parameters
         name = removeTypeTag sourceName

     if any (\p -> statementID (getInitialStatement p) == EXPRESSION_IDENTIFIER) optionalParameters
     then throwSemanticError ("Optional parameters cannot be mixed with required parameters.") (head optionalParameters)
     else return ()

     if any (not . defaultArgumentIsConstant symbols) optionalParameters
     then throwSemanticError ("Default argument values must be constant.") (head optionalParameters)
     else return ()
     
     if Map.lookup name symbols /= Nothing
     then throwSemanticError ("Duplicate identifier '" ++ sourceName ++ "'.") identifier
     else return ()

     if any (== name) (map parameterNameFromStatement parameters)
     then throwSemanticError ("Duplicate identifier '" ++ sourceName ++ "'.") identifier
     else return ()

     addFunction (createLibraryFunction name (readVariableType sourceName) (map (extractParameter symbols) parameters)) name

collectTypeSymbols :: Statement -> CodeTransformation ()
collectTypeSymbols (Statement {statementContents = (Statement {statementContents = (typeName:_)}):fieldNames}) =
  do let sourceName = identifierExpressionValue (getInitialStatement typeName)
         name = map toLower sourceName
         newCustomType = Type {typeName = sourceName,typeSize = 0,typeSymbols = Map.empty}

     if name /= removeTypeTag name
     then throwSemanticError ("Illegal Type name '" ++ sourceName ++ "'.") typeName
     else return ()

     customType <- collectFieldSymbols newCustomType fieldNames 0
     addType customType name

collectFieldSymbols :: Symbol -> [Statement] -> Int -> CodeTransformation Symbol
collectFieldSymbols customType (statement:rest) depth =
  do types <- gets semanticStateTypes
     let typeSourceName = typeName customType
         thisTypeName = map toLower typeSourceName
         fieldName = getInitialStatement statement
         sourceName = identifierExpressionValue (getInitialStatement fieldName)
         name = removeTypeTag (map toLower sourceName)
         symbols = typeSymbols customType
         size = Map.size symbols
         customTypeTag = isolateCustomTypeTag sourceName
         typeContainer = Map.lookup (map toLower customTypeTag) types

     if Map.lookup name (typeSymbols customType) /= Nothing
     then do _ <- throwSemanticError ("Duplicate identifier '" ++ sourceName ++ "'.") fieldName
             return NO_NAMESPACE
     else return NO_NAMESPACE

     let withSymbol = customType {typeSymbols = Map.insert name (Field name (readVariableType sourceName) size) symbols}
     collectFieldSymbols withSymbol rest (depth + 1)

collectFieldSymbols customType _ depth =
  do return (customType {typeSize = depth})

collectDimStatementSymbols :: Statement -> CodeTransformation ()
collectDimStatementSymbols (Statement {statementContents = arrays}) =

  mapM_ collectDimSymbol arrays

collectDimSymbol :: Statement -> CodeTransformation ()
collectDimSymbol array =

  do symbols <- gets semanticStateSymbols
     let (identifier,dimensions) = getFunctionCallOperands (statementContents (getInitialStatement array))
         sourceName = identifierExpressionValue (getInitialStatement identifier)
         name = removeTypeTag (map toLower sourceName)
         symbolContainer = Map.lookup name symbols
         symbol = fromJust symbolContainer

     if symbolContainer /= Nothing
     then do case symbol of
               (Array {arrayName = originalName,arrayType = originalType,arrayNumDimensions = originalNumDimensions}) ->
                 do if length dimensions /= originalNumDimensions
                    then throwSemanticError ("Array '" ++ sourceName ++ "': originally declared as having " ++ show originalNumDimensions ++ " dimensions; now referenced as having " ++ show (length dimensions) ++ " dimensions.") array
                    else return ()
               
               _ -> throwSemanticError ("Duplicate identifier '" ++ sourceName ++ "'.") array

     else addVariable (Array name (VARIABLE_TYPE_ARRAY (readVariableType sourceName)) (length dimensions))

collectLabelSymbol :: Statement -> CodeTransformation ()
collectLabelSymbol statement = 
  do symbols <- gets semanticStateSymbols
     localSymbols <- gets semanticStateLocalSymbols
     nameSpace <- gets semanticStateNameSpace

     let name = map toLower sourceName
         (IdentifierExpression {identifierExpressionValue = sourceName}) = getInitialStatement statement
         globalSymbolContainer = Map.lookup name

     if nameSpace == NO_NAMESPACE
     then do let symbolContainer = Map.lookup name symbols
                 symbol = fromJust symbolContainer
             if symbolContainer /= Nothing
             then throwSemanticError ("Duplicate identifier '" ++ sourceName ++ "'.") statement
             else addLabel (Label (tail name))
     else do let symbolContainer = Map.lookup name localSymbols
                 symbol = fromJust symbolContainer
             if symbolContainer /= Nothing
             then throwSemanticError ("Duplicate identifier '" ++ sourceName ++ "'.") statement
             else addLabel (Label (tail name))

collectGotoLabelSymbol :: Statement -> CodeTransformation ()
collectGotoLabelSymbol statement =

        do let (Statement {statementContents = (destination:_)}) = getInitialStatement statement
               sourceName = identifierExpressionValue destination
               name = map toLower sourceName
           
           trace "collecting goto" (return ())
           symbols <- gets semanticStateSymbols
           localSymbols <- gets semanticStateLocalSymbols
           
           if Map.lookup name symbols == Nothing && Map.lookup name localSymbols == Nothing
           then addLabel (Label name)
           else return ()

insertSemantics :: Statement -> CodeTransformation ()
insertSemantics (Statement {statementID = id,
                            statementContents = (object:expression:_)}) =
  do symbols <- gets semanticStateSymbols
     localSymbols <- gets semanticStateLocalSymbols
     types <- gets semanticStateTypes

     expressionSemantics object
     expressionSemantics expression

     let objectType = getExpressionType object symbols localSymbols types
         expressionType = getExpressionType expression symbols localSymbols types

     if not (isCustomType objectType)
     then throwSemanticError ("'Insert' command can only be applied to user-defined types.") object
     else return ()
            
     if not (isCustomType expressionType)
     then throwSemanticError ("'Insert' command can only be applied to user-defined types.") expression
     else return ()

     if not (objectType == expressionType)
     then throwSemanticError ("Type mismatch (attempt to mix object of type '" ++ customTypeName objectType ++ "' with object of type '" ++ customTypeName expressionType ++ "').") expression           
     else return ()

deleteSemantics :: Statement -> CodeTransformation ()
deleteSemantics (Statement {statementID = STATEMENT_DELETE,
                            statementContents = (object:_)}) =

  do symbols <- gets semanticStateSymbols
     localSymbols <- gets semanticStateLocalSymbols
     types <- gets semanticStateTypes

     expressionSemantics object

     if not (isCustomType (getExpressionType object symbols localSymbols types))
     then throwSemanticError "'Delete' operator can only be applied to user-defined types." object
     else return ()

deleteEachSemantics :: Statement -> CodeTransformation ()
deleteEachSemantics (Statement {statementID = STATEMENT_DELETE_EACH,
                            statementContents = (Statement {statementContents = typeName:_}:_)}) =

  do types <- gets semanticStateTypes
     let sourceName = identifierExpressionValue (getInitialStatement typeName)
         name = map toLower sourceName
         type_ = Map.lookup name types
     if name /= removeTypeTag name
     then throwSemanticError "Expecting type name identifier." typeName
     else return ()
     if type_ == Nothing
     then throwSemanticError ("Reference to undeclared type '" ++ sourceName ++ "'.") typeName
     else return ()

fieldAccessExpressionSemantics :: Statement -> CodeTransformation VariableType
fieldAccessExpressionSemantics (Statement {statementID = EXPRESSION_FIELD_ACCESS,
                                statementContents = (leftOperand:rightOperand:_)}) =
  do symbols <- gets semanticStateSymbols
     localSymbols <- gets semanticStateLocalSymbols
     types <- gets semanticStateTypes
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
                fieldDataType = fieldType field
            
            if variableContainer == Nothing
            then do _ <- throwSemanticError ("Attempt to access field '" ++ sourceFieldName ++ "' of undeclared custom type variable '" ++ sourceName ++ "'.") leftOperand
                    return VARIABLE_TYPE_VOID
            else return VARIABLE_TYPE_VOID
            if isIntrinsicType dataType
            then do _ <- throwSemanticError "Intrinsic type variable treated as custom type variable." leftOperand
                    return VARIABLE_TYPE_VOID
            else return VARIABLE_TYPE_VOID
            if fieldContainer == Nothing
            then do _ <- throwSemanticError ("Type '" ++ typeName type_ ++ "' does not have a field named '" ++ sourceFieldName ++ "'.") rightOperand
                    return VARIABLE_TYPE_VOID
            else return VARIABLE_TYPE_VOID
            return fieldDataType
       EXPRESSION_FIELD_ACCESS ->
         do leftOperandType <- fieldAccessExpressionSemantics leftOperand
            let sourceFieldName = identifierExpressionValue (getInitialStatement rightOperand)
                fieldName = map toLower (removeTypeTag sourceFieldName)
                typeContainer = Map.lookup (map toLower (customTypeName leftOperandType)) types
                type_ = fromJust typeContainer
                typeFields = typeSymbols type_
                fieldContainer = Map.lookup fieldName typeFields
                field = fromJust fieldContainer
                fieldDataType = fieldType field            
            if isIntrinsicType leftOperandType
            then do _ <- throwSemanticError "Intrinsic type field treated as custom type field." leftOperand
                    return VARIABLE_TYPE_VOID
            else return VARIABLE_TYPE_VOID
            if fieldContainer == Nothing
            then do _ <- throwSemanticError ("Type '" ++ typeName type_ ++ "' does not have a field named '" ++ sourceFieldName ++ "'.") rightOperand
                    return VARIABLE_TYPE_VOID
            else return VARIABLE_TYPE_VOID
            return fieldDataType
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

            if variableContainer == Nothing
            then do _ <- throwSemanticError ("Attempt to access field '" ++ sourceFieldName ++ "' of element from undeclared custom type array '" ++ sourceName ++ "'.")  leftOperand
                    return VARIABLE_TYPE_VOID
            else return VARIABLE_TYPE_VOID
            if numArguments /= numDimensions
            then do _ <- throwSemanticError ("Custom type array '" ++ sourceName ++ "': originally declared as having " ++ (show numDimensions) ++ " dimension(s); now referenced as having " ++ (show numArguments) ++ " dimension(s).") identifier
                    return VARIABLE_TYPE_VOID
            else do if hasTypeTag sourceName && readVariableType (map toLower sourceName) /= targetType dataType
                    then do _ <- throwSemanticError ("Duplicate identifier '" ++ sourceName ++ "'.") identifier
                            return VARIABLE_TYPE_VOID
                    else do _ <- mapM_ expressionSemantics arguments
                            return VARIABLE_TYPE_VOID

            if fieldContainer == Nothing
            then do _ <- throwSemanticError ("Type '" ++ typeName type_ ++ "' does not have a field named '" ++ sourceFieldName ++ "'.") rightOperand
                    return VARIABLE_TYPE_VOID            
            else return VARIABLE_TYPE_VOID

expressionSemantics :: Statement -> CodeTransformation ()
expressionSemantics (Statement {statementID = STATEMENT_EXPRESSION,
                                statementContents = (expression:_)}) =
  
  do expressionSemantics expression

expressionSemantics expression =
  do case statementID expression of
       EXPRESSION_IDENTIFIER -> identifierExpressionSemantics expression
       EXPRESSION_NEG -> negExpressionSemantics expression
       EXPRESSION_NEW -> newExpressionSemantics expression
       EXPRESSION_ASSIGN -> assignmentExpressionSemantics expression
       EXPRESSION_INT_CONSTANT -> intConstantExpressionSemantics expression
       EXPRESSION_FLOAT_CONSTANT -> floatConstantExpressionSemantics expression
       EXPRESSION_STRING_CONSTANT -> stringConstantExpressionSemantics expression
       EXPRESSION_HEX_CONSTANT -> hexConstantExpressionSemantics expression
       EXPRESSION_BIN_CONSTANT -> binConstantExpressionSemantics expression
       EXPRESSION_PI -> piExpressionSemantics expression
       EXPRESSION_NULL -> nullExpressionSemantics expression
       _ -> do if statementID expression `elem` typeListOperatorList
               then typeListOperatorExpressionSemantics expression
               else genericExpressionSemantics expression

identifierExpressionSemantics (Statement {statementID = EXPRESSION_IDENTIFIER, 
                                statementLineNumber = lineNumber,
                                statementOffset = offset,
                                statementContents = (identifier:_)}) =
  do symbols <- gets semanticStateSymbols
     localSymbols <- gets semanticStateLocalSymbols
     nameSpace <- gets semanticStateNameSpace
     let statement = Statement EXPRESSION_IDENTIFIER lineNumber offset [identifier]
         sourceName = identifierExpressionValue identifier
         name = removeTypeTag (map toLower sourceName)
         symbolContainer = Map.lookup name symbols
         localSymbolContainer = Map.lookup name localSymbols

     if nameSpace == NO_NAMESPACE
     then do let symbol = fromJust symbolContainer
             if symbolContainer == Nothing
             then do addVariable
                      (Variable {variableName = name,
                                 variableType = (readVariableType sourceName),
                                 variableIsGlobal = False})
             else do checkSymbolUsage symbol sourceName statement
     else do let symbol = fromJust symbolContainer
                 localSymbol = fromJust localSymbolContainer
                 offset = (-8) * ((numPreservedRegisters - 1) + functionNumLocals nameSpace + 1)

             if localSymbolContainer == Nothing
             then if symbolContainer == Nothing || (not (isGlobal symbol || isArray symbol || isConst symbol))
                  then do addLocalAutomaticVariable
                            (LocalAutomaticVariable {localAutomaticVariableName = sourceName,
                                                     localAutomaticVariableType = (readVariableType sourceName),
                                                     localAutomaticVariableIsArgument = False,
                                                     localAutomaticVariableAddress = offset})
                  else do checkSymbolUsage symbol sourceName statement
             else do checkSymbolUsage localSymbol sourceName statement

  where checkSymbolUsage symbol sourceName statement =
          do case symbol of
               (Array {arrayType = VARIABLE_TYPE_ARRAY {targetType = dataType}}) ->

                  throwSemanticError ("Duplicate identifier '" ++ sourceName ++ "'.") statement

               (Variable {variableType = dataType}) ->

                  if hasTypeTag sourceName && dataType /= readVariableType sourceName
                  then throwSemanticError ("Duplicate identifier '" ++ sourceName ++ "'.") statement
                  else do return ()

               (LocalAutomaticVariable {localAutomaticVariableType = dataType}) ->

                  if hasTypeTag sourceName && dataType /= readVariableType sourceName
                  then throwSemanticError ("Duplicate identifier '" ++ sourceName ++ "'.") statement
                  else do return ()

               Const {constType = dataType} -> 
                  
                  if hasTypeTag sourceName && dataType /= readVariableType sourceName
                  then throwSemanticError ("Duplicate identifier '" ++ sourceName ++ "'.") statement
                  else do return ()

               Function {functionName = sourceName} -> 
                  
                  throwSemanticError ("Illegal use of function name '" ++ sourceName ++ "'.") statement
                        
               k -> error "1"

negExpressionSemantics (Statement {statementID = EXPRESSION_NEG,
                                statementContents = (negate:_)}) =
        do let operand = getUnaryOperand (statementContents negate)
           symbols <- gets semanticStateSymbols
           localSymbols <- gets semanticStateLocalSymbols

           if expressionIsConstant symbols operand
           then do let reduced = reduceConstantExpression negate symbols localSymbols
                   case reduced of
                     (Statement {statementID = EXPRESSION_INT_CONSTANT,
                                 statementContents = (IntConstantExpression {intConstantExpressionValue = value}:_)}) ->
                        do ints <- gets semanticStateInts
                           if Map.lookup (-value) ints == Nothing
                           then do addInt (-value)
                           else return ()
                     (Statement {statementID = EXPRESSION_FLOAT_CONSTANT,
                                 statementContents = (FloatConstantExpression {floatConstantExpressionValue = value}:_)}) ->
                        do floats <- gets semanticStateFloats
                           if Map.lookup ((-value)) floats == Nothing
                           then do addFloat ((-value))
                           else return ()
           else return ()

newExpressionSemantics (Statement {statementID = EXPRESSION_NEW,
                                statementContents = (operand:_)}) =
  do if statementID operand /= EXPRESSION_IDENTIFIER
     then throwSemanticError "Expecting type name identifier." operand
     else return ()
     types <- gets semanticStateTypes
     let sourceName = identifierExpressionValue (getInitialStatement operand)
         name = map toLower sourceName
         type_ = Map.lookup name types
     if name /= removeTypeTag name
     then throwSemanticError "Expecting type name identifier." operand
     else return ()
     if type_ == Nothing
     then throwSemanticError ("Reference to undeclared type '" ++ sourceName ++ "'.") operand
     else return ()

typeListOperatorExpressionSemantics (Statement {statementID = id,
                                statementContents = (operand:_)}) =
  do symbols <- gets semanticStateSymbols
     localSymbols <- gets semanticStateLocalSymbols
     types <- gets semanticStateTypes
              
     case id of

       EXPRESSION_FIRST ->
         do let sourceTypeName = identifierExpressionValue (getInitialStatement operand)
                typeName = map toLower sourceTypeName
                type_ = Map.lookup typeName types

            if typeName /= removeTypeTag typeName
            then throwSemanticError "Illegal Type name." operand
            else return ()
            if type_ == Nothing
            then throwSemanticError ("Reference to undeclared type '" ++ sourceTypeName ++ "'.") operand
            else return ()
         
       EXPRESSION_LAST ->
         do let sourceTypeName = identifierExpressionValue (getInitialStatement operand)
                typeName = map toLower sourceTypeName
                type_ = Map.lookup typeName types

            if typeName /= removeTypeTag typeName
            then throwSemanticError "Illegal Type name." operand
            else return ()
            if type_ == Nothing
            then throwSemanticError ("Reference to undeclared type '" ++ sourceTypeName ++ "'.") operand
            else return ()

       EXPRESSION_BEFORE ->
         do expressionSemantics operand
            if not (isCustomType (getExpressionType operand symbols localSymbols types))
            then throwSemanticError "'Before' operator can only be applied to user-defined types." operand
            else return ()
       
       EXPRESSION_AFTER ->
         do expressionSemantics operand 
            if not (isCustomType (getExpressionType operand symbols localSymbols types))
            then throwSemanticError "'After' operator can only be applied to user-defined types." operand
            else return () 

assignmentExpressionSemantics expression =
  do let (leftOperand,rightOperand) = getBinaryOperands (statementContents expression)

     expressionSemantics leftOperand
     expressionSemantics rightOperand

     checkOperatorTypeCombination expression

     symbols <- gets semanticStateSymbols
     localSymbols <- gets semanticStateLocalSymbols
     types <- gets semanticStateTypes

     let leftOperandType = getExpressionType leftOperand symbols localSymbols types
         rightOperandType = getExpressionType rightOperand symbols localSymbols types

     if isCustomType leftOperandType && isCustomType rightOperandType
     then do let leftOperandTypeName = map toLower (removeTypeTag (customTypeName leftOperandType))
                 rightOperandTypeName = map toLower (removeTypeTag (customTypeName rightOperandType))
             if leftOperandTypeName /= rightOperandTypeName && (not (customTypeName rightOperandType == "Null"))
             then throwSemanticError ("Type mismatch (attempt to assign value of type '" ++ customTypeName rightOperandType ++ "' to variable of type '" ++ customTypeName leftOperandType ++ "').") rightOperand
             else do return ()
     else do return ()

intConstantExpressionSemantics (Statement {statementID = EXPRESSION_INT_CONSTANT,
                                statementContents = (intConstant:_)}) =
        do let value = intConstantExpressionValue intConstant
           ints <- gets semanticStateInts
           if Map.lookup value ints == Nothing
           then addInt value
           else return ()

floatConstantExpressionSemantics (Statement {statementID = EXPRESSION_FLOAT_CONSTANT,
                                statementContents = (floatConstant:_)}) =
        do let value = floatConstantExpressionValue floatConstant
           floats <- gets semanticStateFloats
           if Map.lookup value floats == Nothing
           then addFloat value
           else return ()

stringConstantExpressionSemantics (Statement {statementID = EXPRESSION_STRING_CONSTANT,
                                statementContents = (stringConstant:_)}) =
        do let value = stringConstantExpressionValue stringConstant
           strings <- gets semanticStateStrings
           if Map.lookup value strings == Nothing
           then addString value
           else return ()

hexConstantExpressionSemantics (Statement {statementID = EXPRESSION_HEX_CONSTANT,
                                statementContents = (hexConstant:_)}) =
        do let value = hexToInt (hexConstantExpressionValue hexConstant)
           ints <- gets semanticStateInts
           if Map.lookup value ints == Nothing
           then addInt value
           else return ()

binConstantExpressionSemantics (Statement {statementID = EXPRESSION_BIN_CONSTANT,
                                statementContents = (binConstant:_)}) =
        do let value = binToInt (binConstantExpressionValue binConstant)
           ints <- gets semanticStateInts
           if Map.lookup value ints == Nothing
           then addInt value
           else return ()

piExpressionSemantics (Statement {statementID = EXPRESSION_PI}) =
        do let value = 3.14159265358979323
           floats <- gets semanticStateFloats
           if Map.lookup value floats == Nothing
           then addFloat value
           else return ()

trueExpressionSemantics (Statement {statementID = EXPRESSION_TRUE}) =
        do let value = 1
           ints <- gets semanticStateInts
           if Map.lookup value ints == Nothing
           then addInt value
           else return ()

falseExpressionSemantics (Statement {statementID = EXPRESSION_FALSE}) =
        do let value = 0
           ints <- gets semanticStateInts
           if Map.lookup value ints == Nothing
           then addInt value
           else return ()

nullExpressionSemantics (Statement {statementID = EXPRESSION_NULL}) =
        do let value = 0
           ints <- gets semanticStateInts
           if Map.lookup value ints == Nothing
           then addInt value
           else return ()

genericExpressionSemantics expression
  | statementID expression == EXPRESSION_FIELD_ACCESS =
    do _ <- fieldAccessExpressionSemantics expression
       return ()
  | expressionIsAtomic expression =
    return ()
  | expressionIsUnary expression =
      do let operand = getUnaryOperand (statementContents expression)
         symbols <- gets semanticStateSymbols
         localSymbols <- gets semanticStateLocalSymbols
         if expressionIsConstant symbols expression
         then do let constant = reduceConstantExpression expression symbols localSymbols
                 case getExpressionType expression symbols localSymbols Map.empty of
                   VARIABLE_TYPE_INT -> addInt (intConstantExpressionValue (getInitialStatement constant))
                   VARIABLE_TYPE_FLOAT ->  addFloat (floatConstantExpressionValue (getInitialStatement constant))
                   VARIABLE_TYPE_STRING -> addString (stringConstantExpressionValue (getInitialStatement constant))
         else do expressionSemantics operand

  | expressionIsBinary expression =
    do symbols <- gets semanticStateSymbols
       localSymbols <- gets semanticStateLocalSymbols
       types <- gets semanticStateTypes
       let (leftOperand, rightOperand) = getBinaryOperands (statementContents expression)
           leftOperandType = getExpressionType leftOperand symbols localSymbols types
           rightOperandType = getExpressionType rightOperand symbols localSymbols types

       if expressionIsConstant symbols expression
       then do checkOperatorTypeCombination expression
               let constant = reduceConstantExpression expression symbols localSymbols
               case getExpressionType expression symbols localSymbols Map.empty of
                 VARIABLE_TYPE_INT -> addInt (intConstantExpressionValue (getInitialStatement constant))
                 VARIABLE_TYPE_FLOAT ->  addFloat (floatConstantExpressionValue (getInitialStatement constant))
                 VARIABLE_TYPE_STRING -> addString (stringConstantExpressionValue (getInitialStatement constant))
       else do expressionSemantics leftOperand
               expressionSemantics rightOperand
               checkOperatorTypeCombination expression

  | expressionIsFunctionCall expression =

    do symbols <- gets semanticStateSymbols
       localSymbols <- gets semanticStateLocalSymbols
       types <- gets semanticStateTypes

       let (identifier,arguments) = getFunctionCallOperands (statementContents expression)
           sourceName = identifierExpressionValue (getInitialStatement identifier)
           numArguments = length arguments
           name = removeTypeTag (map toLower sourceName)
           symbolContainer = Map.lookup name symbols
           symbol = fromJust symbolContainer

       if symbolContainer == Nothing
       then throwSemanticError ("Reference to undeclared function '" ++ sourceName ++ "'.") expression
       else return ()

       case symbol of

         (Function {functionType = dataType, functionMaxNumArguments = maxNumArguments, functionMinNumArguments = minNumArguments}) ->

           do if not (standardFunctionIsUsed name symbols)
              then markStandardFunctionAsUsed name
              else return ()

              if numArguments < minNumArguments || numArguments > maxNumArguments
              then if minNumArguments == maxNumArguments
                   then throwSemanticError ("Function '" ++ sourceName ++ "': originally declared as having " ++ show minNumArguments ++ " parameter(s); now referenced as having " ++ (show numArguments) ++ " parameter(s).") expression
                   else throwSemanticError ("Function '" ++ sourceName ++ "': originally declared as having between " ++ show minNumArguments ++ " and " ++ show maxNumArguments ++ " parameter(s); now referenced as having " ++ (show numArguments) ++ " parameter(s).") expression
              else mapM_ expressionSemantics arguments
         
         (Array name dataType numDimensions) ->

           do if numArguments /= numDimensions
              then throwSemanticError ("Array '" ++ name ++ "': originally declared as having " ++ (show numDimensions) ++ " dimension(s); now referenced as having " ++ (show numArguments) ++ " dimension(s).") identifier
              else do if hasTypeTag sourceName && readVariableType (map toLower sourceName) /= targetType dataType
                      then throwSemanticError ("Duplicate identifier '" ++ sourceName ++ "'. " ++ show (readVariableType sourceName) ++ " " ++ show (targetType dataType)) identifier
                      else do mapM_ expressionSemantics arguments

         (Variable {variableName = name,variableType = dataType}) ->

           do throwSemanticError ("Intrinsic type variable '" ++ name ++ "' incorrectly treated as function/array.") identifier

  | otherwise = error (show expression)

functionCallIsNotTypeSafe :: [VariableType] -> [VariableType] -> Bool
functionCallIsNotTypeSafe originalTypes newTypes =
  any comparison (zip originalTypes newTypes)
  where comparison (orig,new) =
          if isCustomType orig && isCustomType new && customTypeName orig /= customTypeName new
          then True
          else if any isCustomType [orig,new] && any (not . isCustomType) [orig,new]
               then True
               else False

checkOperatorTypeCombination :: Statement -> CodeTransformation ()
checkOperatorTypeCombination expression =
  
  do symbols <- gets semanticStateSymbols
     localSymbols <- gets semanticStateLocalSymbols
     types <- gets semanticStateTypes
     if expressionIsBinary expression
     then do let (leftOperand,rightOperand) = getBinaryOperands (statementContents expression)
                 leftOperandType = getExpressionType leftOperand symbols localSymbols types
                 rightOperandType = getExpressionType rightOperand symbols localSymbols types

             if not (expressionIsAtomic leftOperand || statementID leftOperand == EXPRESSION_FIELD_ACCESS)
             then checkOperatorTypeCombination leftOperand
             else return ()

             if not (expressionIsAtomic rightOperand || statementID rightOperand == EXPRESSION_FIELD_ACCESS)
             then checkOperatorTypeCombination rightOperand
             else return ()

             if ((expressionIsAtomic leftOperand && isCustomType leftOperandType) || (expressionIsAtomic rightOperand && isCustomType rightOperandType)) && statementID expression `notElem` [EXPRESSION_ASSIGN,EXPRESSION_FIELD_ACCESS,EXPRESSION_EQUAL_TO,EXPRESSION_NOT_EQUAL_TO]
             then throwSemanticError ("Illegal operation.") leftOperand
             else return ()

             if leftOperandType == VARIABLE_TYPE_STRING && rightOperandType == VARIABLE_TYPE_STRING && statementID expression `notElem` (relationalOperatorList ++ [EXPRESSION_ADD] ++ [EXPRESSION_ASSIGN])
             then throwSemanticError ("Operator cannot be applied to strings.") leftOperand
             else return ()

             if isCustomType leftOperandType && isCustomType rightOperandType
             then do let leftOperandTypeName = map toLower (removeTypeTag (customTypeName leftOperandType))
                         rightOperandTypeName = map toLower (removeTypeTag (customTypeName rightOperandType))
                     if leftOperandTypeName /= rightOperandTypeName && (not (customTypeName rightOperandType == "Null"))
                     then throwSemanticError ("Type mismatch (attempt to compare/assign variable of type '" ++ customTypeName rightOperandType ++ "' to variable of type '" ++ customTypeName leftOperandType ++ "'.") rightOperand
                     else do return ()
             else do return ()
                     if any (\t -> isCustomType t) [leftOperandType,rightOperandType] && (not (any (== VARIABLE_TYPE_VOID) [leftOperandType,rightOperandType]))
                     then throwSemanticError ("Type mismatch (attempt to mix user defined type with intrinsic type).") rightOperand
                     else do return ()
     else do let operand = getUnaryOperand (statementContents expression)
                 dataType = getExpressionType operand symbols localSymbols types
             if statementID expression `notElem` (EXPRESSION_NEW:typeListOperatorList)
             then if dataType == VARIABLE_TYPE_STRING && statementID expression `notElem` (EXPRESSION_GROUP:EXPRESSION_FUNCTION_CALL:typeConversionOperatorList)
                  then throwSemanticError ("Operator cannot be applied to strings.") operand
                  else return ()
             else return ()

dataSemantics :: Statement -> CodeTransformation ()
dataSemantics (Statement {statementContents = expressionList}) =

  do symbols <- gets compileStateSymbols
     mapM_ (requireConstantExpression symbols) expressionList
     mapM_ expressionSemantics expressionList

requireConstantExpression :: SymbolTable -> Statement -> CodeTransformation ()
requireConstantExpression globalSymbols statement =

        do if not (expressionIsConstant globalSymbols statement)
           then throwSemanticError ("Expression is not constant.") statement
           else return ()

requireLabel :: Statement -> CodeTransformation ()
requireLabel statement =

        do if not (expressionIsLabel statement)
           then throwSemanticError ("Expression is not a label.") statement
           else return ()

readSemantics :: Statement -> CodeTransformation ()
readSemantics (Statement {statementContents = variableList}) =

        do mapM_ expressionSemantics variableList
     
forSemantics :: Statement -> CodeTransformation ()
forSemantics (Statement {statementContents = statements}) =

        do incrementLoopDepth
           symbols <- gets semanticStateSymbols
           localSymbols <- gets semanticStateLocalSymbols
           types <- gets semanticStateTypes

           let (Statement {statementContents = (initialiserExpression:limitExpression:rest)}:code:_) = statements
               initialiser = getInitialStatement initialiserExpression
               limit = getInitialStatement limitExpression
               (leftOperand, _) = getBinaryOperands (statementContents initialiser)
               sourceCounterName = getIdentifierValue leftOperand
               counterName = map toLower (removeTypeTag sourceCounterName)
               step = head (statementContents (head rest))
               constantNonZeroStep =
                 if rest /= []
                 then if expressionIsConstant symbols step || statementID step == EXPRESSION_STRING_CONSTANT || isStringExpression step symbols localSymbols types
                      then let reduced = reduceConstantExpression step symbols localSymbols in
                           case statementID reduced of
                                EXPRESSION_INT_CONSTANT -> getIntConstantValue reduced /= 0
                                EXPRESSION_FLOAT_CONSTANT -> getFloatConstantValue reduced /= 0.0
                                EXPRESSION_STRING_CONSTANT -> any (\ c -> not (c `elem` ".0")) (removeQuotes (getStringConstantValue reduced))
                      else False
                 else True
           
           expressionSemantics initialiser
           expressionSemantics limit
           if rest /= []
           then expressionSemantics step
           else return ()

           if statementID leftOperand /= EXPRESSION_IDENTIFIER
           then throwSemanticError "Loop counter must be a variable." leftOperand
           else return ()

           if not constantNonZeroStep
           then throwSemanticError "Step value must be constant and non-zero." step
           else return ()

           codeSemantics code
           decrementLoopDepth

forEachSemantics :: Statement -> CodeTransformation ()
forEachSemantics (Statement {statementContents = statements}) =

        do incrementLoopDepth
           symbols <- gets semanticStateSymbols
           localSymbols <- gets semanticStateLocalSymbols
           types <- gets semanticStateTypes
           nameSpace <- gets semanticStateNameSpace

           let (Statement {statementContents = (initialiserExpression:_)}:code:_) = statements
               initialiser = getInitialStatement initialiserExpression
               (leftOperand,rightOperand) = getBinaryOperands (statementContents initialiser)
               sourceIteratorName = getIdentifierValue leftOperand
               iteratorName = map toLower (removeTypeTag sourceIteratorName)
               sourceTypeName = getIdentifierValue rightOperand
               typeName = map toLower sourceTypeName
               localVariableContainer = Map.lookup iteratorName localSymbols
               globalVariableContainer = Map.lookup iteratorName symbols
               variableContainer =
                 if localVariableContainer /= Nothing
                 then localVariableContainer
                 else if globalVariableContainer /= Nothing
                      then globalVariableContainer
                      else Nothing

           if statementID leftOperand /= EXPRESSION_IDENTIFIER
           then throwSemanticError "Loop iterator must be a variable." leftOperand
           else return ()

           if sourceTypeName /= removeTypeTag sourceTypeName
           then throwSemanticError ("Illegal Type name '" ++ sourceTypeName ++ "'.") rightOperand
           else return ()

           if variableContainer == Nothing
           then do if not (isCustomType (readVariableType sourceIteratorName))
                   then throwSemanticError "Loop iterator must have a user-defined type." leftOperand
                   else do if map toLower (customTypeName (readVariableType sourceIteratorName)) /= typeName
                           then throwSemanticError ("Type mismatch (iterator has type '" ++ (isolateCustomTypeTag sourceIteratorName) ++ "'; loop is of type '" ++ sourceTypeName ++ "'.") leftOperand
                           else do if nameSpace == NO_NAMESPACE
                                    then do addVariable
                                              (Variable {variableName = iteratorName,
                                                         variableType = readVariableType sourceIteratorName,
                                                         variableIsGlobal = False})
                                    else do let offset = (-8) * ((numPreservedRegisters - 1) + functionNumLocals nameSpace + 1)
                                            addLocalAutomaticVariable
                                              (LocalAutomaticVariable {localAutomaticVariableName = sourceIteratorName,
                                                                       localAutomaticVariableType = (readVariableType sourceIteratorName),
                                                                       localAutomaticVariableIsArgument = False,
                                                                       localAutomaticVariableAddress = offset})
           else do let type_ =
                         case fromJust variableContainer of
                           Variable {variableType = t} -> t
                           LocalAutomaticVariable {localAutomaticVariableType = t} -> t
                   if not (isCustomType type_ )
                   then throwSemanticError "Loop iterator must have a user-defined type." leftOperand
                   else return ()
                   if map toLower (customTypeName type_) /= typeName
                   then throwSemanticError ("Type mismatch (iterator has type '" ++ customTypeName (variableType (fromJust variableContainer)) ++ "'; loop is of type '" ++ sourceTypeName ++ "'.") leftOperand
                   else return ()
              
           codeSemantics code
           decrementLoopDepth

whileSemantics :: Statement -> CodeTransformation ()
whileSemantics (Statement {statementContents = (expression:code:_)}) =
  do incrementLoopDepth
     expressionSemantics expression
     codeSemantics code
     decrementLoopDepth

repeatSemantics :: Statement -> CodeTransformation ()
repeatSemantics (Statement {statementContents = (code:footer:_)}) =
  do incrementLoopDepth
     if statementID footer == STATEMENT_FOREVER
     then return ()
     else expressionSemantics ((head . statementContents) footer)
     codeSemantics code
     decrementLoopDepth

incrementLoopDepth :: CodeTransformation ()
incrementLoopDepth =
  do state <- get
     put state {semanticStateLoopDepth = (semanticStateLoopDepth state) + 1}

decrementLoopDepth :: CodeTransformation ()
decrementLoopDepth =
  do state <- get
     put state {semanticStateLoopDepth = (semanticStateLoopDepth state) - 1}

exitSemantics :: Statement -> CodeTransformation ()
exitSemantics statement =
  do depth <- gets semanticStateLoopDepth
     if depth == 0
     then throwSemanticError ("Exit without For/While/Repeat.") statement
     else return ()

ifSemantics :: Statement -> CodeTransformation ()
ifSemantics statement =
        do elseIfSemantics (statementContents statement)

elseIfSemantics :: [Statement] -> CodeTransformation ()
elseIfSemantics (statement:rest) =
  do case statementID statement of
          STATEMENT_IF_DEFAULT ->
            do let (predicate:code:_) = statementContents statement
               expressionSemantics predicate
               codeSemantics code
          STATEMENT_ELSE_IF ->
            do let (predicate:code:_) = statementContents statement
               expressionSemantics predicate
               codeSemantics code
          STATEMENT_ELSE ->
            do let (code:_) = statementContents statement
               codeSemantics code
     elseIfSemantics rest

elseIfSemantics _ =
  do return ()

selectSemantics :: Statement -> CodeTransformation ()
selectSemantics (Statement {statementContents = (expression:rest)}) =
        do expressionSemantics expression
           caseSemantics rest

caseSemantics :: [Statement] -> CodeTransformation ()
caseSemantics (statement:rest) =
  do case statementID statement of
          STATEMENT_CASE ->
            do let (predicate:code:_) = statementContents statement
               expressionSemantics predicate
               codeSemantics code
          STATEMENT_DEFAULT ->
            do let (code:_) = statementContents statement
               codeSemantics code
     caseSemantics rest

caseSemantics _ =
  do return ()

labelSemantics :: Statement -> CodeTransformation ()
labelSemantics statement =
  
  do let name = map toLower sourceName
         (IdentifierExpression {identifierExpressionValue = sourceName}) = getInitialStatement statement
     symbols <- gets semanticStateSymbols
     localSymbols <- gets semanticStateLocalSymbols
           
     if Map.lookup name symbols == Nothing && Map.lookup name localSymbols == Nothing
     then do addLabel (Label (tail name))
     else throwSemanticError ("Duplicate label '" ++ show name ++ "'.") statement

dataLabelSemantics = labelSemantics

onSemantics :: Statement -> CodeTransformation ()
onSemantics (Statement {statementID = id,statementContents = (predicate:destinationList)}) =
  
  do nameSpace <- gets semanticStateNameSpace
     if nameSpace == NO_NAMESPACE
     then expressionSemantics predicate
     else if id == STATEMENT_ON_GOSUB
          then throwSemanticError "On... Gosub cannot be used inside functions." (head destinationList)
          else expressionSemantics predicate

gotoSemantics :: Statement -> CodeTransformation ()
gotoSemantics statement =

        do let (Statement {statementContents = (destination:_)}) = getInitialStatement statement
               sourceName = identifierExpressionValue destination
               name = map toLower sourceName

           symbols <- gets semanticStateSymbols
           localSymbols <- gets semanticStateLocalSymbols
            
           if Map.lookup name symbols == Nothing && Map.lookup name localSymbols == Nothing
           then throwSemanticError ("Reference to undefined label '" ++ sourceName ++ "'.") statement
           else return ()

gosubSemantics = gotoSemantics
restoreSemantics = gotoSemantics

collectFunctionSymbols :: Statement -> CodeTransformation ()
collectFunctionSymbols (Statement {statementContents = statements}) =

        do symbols <- gets semanticStateSymbols
           localSymbols <- gets semanticStateLocalSymbols
           
           let (Statement {statementContents = (identifier:_)}:prototype:definition:_) = statements
               sourceName = identifierExpressionValue (getInitialStatement identifier)
               parameters = statementContents prototype
               requiredParameters = takeWhile (\p -> statementID (getInitialStatement p) == EXPRESSION_IDENTIFIER) parameters
               optionalParameters = dropWhile (\p -> statementID (getInitialStatement p) == EXPRESSION_IDENTIFIER) parameters
               name = removeTypeTag (map toLower sourceName)               
           
           mapM_ expressionSemantics optionalParameters
           
           if any (\p -> statementID (getInitialStatement p) == EXPRESSION_IDENTIFIER) optionalParameters
           then throwSemanticError ("Optional parameters cannot be mixed with required parameters.") (head optionalParameters)
           else return ()

           if any (not . defaultArgumentIsConstant symbols) optionalParameters
           then throwSemanticError ("Default argument values must be constant.") (head optionalParameters)
           else return ()

           if Map.lookup name symbols /= Nothing
           then throwSemanticError ("Duplicate identifier '" ++ sourceName ++ "'.") identifier
           else return ()

           if any (== name) (map parameterNameFromStatement parameters)
           then throwSemanticError ("Duplicate identifier '" ++ sourceName ++ "'.") identifier
           else return ()

           let combinedArguments = zip (map (extractParameter symbols) parameters) parameters
#if LINUX==1
               registerArguments = extractRegisterArguments combinedArguments 0 0 0
               stackArguments = extractStackArguments combinedArguments 0 0 0
#elif WINDOWS==1
               registerArguments = extractRegisterArguments combinedArguments
               stackArguments = extractStackArguments combinedArguments
#endif
               registerSymbols = (zip (map parameterNameFromStatement (map snd registerArguments)) (map extractSymbol (zip (map snd registerArguments) registerArgumentOffsets)))
               stackSymbols = (zip (map parameterNameFromStatement (map snd stackArguments)) (map extractSymbol (zip (map snd stackArguments) stackArgumentOffsets)))
               functionSymbolTable = Map.fromList (registerSymbols ++ stackSymbols)

               registerArgumentOffsets = map (((-8) *) . (numPreservedRegisters +)) (take (length registerArguments) [0,1..])
               stackArgumentOffsets = map ((8 *) . (2 + sizeOfShadowSpace +)) (take (length stackArguments) [0,1..])

           let function = createUserFunction name (readVariableType sourceName) (map (extractParameter symbols) parameters) (length requiredParameters + length optionalParameters) (length requiredParameters) (length registerArguments) (length stackArguments) (length registerArguments) functionSymbolTable
           collectSymbols definition
           addFunction function name

functionSemantics :: Statement -> CodeTransformation ()
functionSemantics (Statement {statementContents = statements}) =

  do symbols <- gets semanticStateSymbols
     localSymbols <- gets semanticStateLocalSymbols

     let (Statement {statementContents = (identifier:_)}:prototype:code:_) = statements
         sourceName = identifierExpressionValue (getInitialStatement identifier)
         name = removeTypeTag (map toLower sourceName)               
         functionContainer = Map.lookup name symbols
         function = fromJust functionContainer
         functionSymbolTable = functionSymbols function
         
     originalNameSpace <- gets semanticStateNameSpace
     setSemanticStateNameSpace function
     setSemanticStateLocalSymbols functionSymbolTable
     codeSemantics code
     setSemanticStateLocalSymbols localSymbols
     setSemanticStateNameSpace originalNameSpace

parameterNameFromStatement :: Statement -> String                 
parameterNameFromStatement statement =
  case statementID (getInitialStatement statement) of
    EXPRESSION_IDENTIFIER -> removeTypeTag (map toLower (getIdentifierValue statement))
    EXPRESSION_EQUAL_TO -> 
      let (leftOperand,_) = getBinaryOperands (statementContents (getInitialStatement statement)) in
      removeTypeTag (map toLower (getIdentifierValue (leftOperand)))

parameterTypeFromStatement :: Statement -> VariableType
parameterTypeFromStatement statement =
  case statementID (getInitialStatement statement) of
    EXPRESSION_IDENTIFIER -> readVariableType (getIdentifierValue statement)
    EXPRESSION_EQUAL_TO -> 
      let (leftOperand,_) = getBinaryOperands (statementContents (getInitialStatement statement)) in
      readVariableType (getIdentifierValue leftOperand)

extractParameter :: SymbolTable -> Statement -> Parameter
extractParameter symbols statement =
  case statementID (getInitialStatement statement) of
    EXPRESSION_IDENTIFIER ->
      Parameter (parameterNameFromStatement statement) (parameterTypeFromStatement statement) EmptyStatement
    EXPRESSION_EQUAL_TO -> 
      let (_,rightOperand) = getBinaryOperands (statementContents (getInitialStatement statement)) in
      Parameter (parameterNameFromStatement statement) (parameterTypeFromStatement statement) (reduceConstantExpression rightOperand symbols Map.empty)

extractSymbol :: (Statement,Int) -> Symbol
extractSymbol (statement, address) =
  LocalAutomaticVariable {localAutomaticVariableName = (parameterNameFromStatement statement),
                          localAutomaticVariableType = (parameterTypeFromStatement statement),
                          localAutomaticVariableIsArgument = True,
                          localAutomaticVariableAddress = address}
  
defaultArgumentIsConstant :: SymbolTable -> Statement -> Bool
defaultArgumentIsConstant symbols parameter =
  let (_,rightOperand) = getBinaryOperands (statementContents (getInitialStatement parameter)) in
  expressionIsConstant symbols rightOperand

collectConstSymbols :: Statement -> CodeTransformation ()
collectConstSymbols Statement {statementContents = expressionList} =
  do mapM_ constDeclarationSemantics expressionList

constDeclarationSemantics :: Statement -> CodeTransformation ()
constDeclarationSemantics Statement {statementContents = (expression:_)} =
  do if statementID expression /= EXPRESSION_ASSIGN
     then throwSemanticError ("Expecting constant assignment expression.") expression
     else return ()
     symbols <- gets semanticStateSymbols
     nameSpace <- gets semanticStateNameSpace
     if nameSpace == NO_NAMESPACE
     then do let (leftOperand,rightOperand) = getBinaryOperands (statementContents expression)
                 sourceName = identifierExpressionValue (getInitialStatement leftOperand)
                 name = removeTypeTag (map toLower sourceName)
                 container = Map.lookup name symbols
             
             requireConstantExpression symbols rightOperand    
                        
             if container /= Nothing
             then throwSemanticError ("Duplicate identifier '" ++ sourceName ++ "'.") expression
             else return ()
             
             let value = ConstValue (reduceConstantExpression rightOperand symbols Map.empty)

             addConst (Const {constName = sourceName,
                       constType = (readVariableType sourceName),
                       constValue = value})

     else throwSemanticError ("Attempt to declare constant in non-global scope.") expression 

globalSemantics :: Statement -> CodeTransformation ()
globalSemantics Statement {statementContents = expressionList} =
  do mapM_ globalVariableDeclarationSemantics expressionList

globalVariableDeclarationSemantics :: Statement -> CodeTransformation ()
globalVariableDeclarationSemantics Statement {statementContents = (expression:_)} =
  do symbols <- gets semanticStateSymbols
     nameSpace <- gets semanticStateNameSpace
     types <- gets semanticStateTypes
     if nameSpace == NO_NAMESPACE
     then case statementID expression of
            EXPRESSION_IDENTIFIER ->
              do let sourceName = identifierExpressionValue ((head . statementContents) expression)
                     name = removeTypeTag (map toLower sourceName)
                     container = Map.lookup name symbols
                     typeContainer = Map.lookup (map toLower (isolateCustomTypeTag sourceName)) types

                 if container /= Nothing
                 then throwSemanticError ("Duplicate identifier '" ++ sourceName ++ "'.") expression
                 else return ()

                 if isCustomType (readVariableType sourceName) && typeContainer == Nothing
                 then throwSemanticError ("Reference to undeclared type '" ++ isolateCustomTypeTag sourceName ++ "'.") expression
                 else return ()

                 addVariable (Variable {variableName = sourceName,
                                        variableType = (readVariableType sourceName),
                                        variableIsGlobal = True})
            EXPRESSION_ASSIGN ->

              do let (leftOperand,rightOperand) = getBinaryOperands (statementContents expression)
                     sourceName = identifierExpressionValue (getInitialStatement leftOperand)
                     name = removeTypeTag (map toLower sourceName)
                     container = Map.lookup name symbols
                     typeContainer = Map.lookup (map toLower (isolateCustomTypeTag sourceName)) types

                 if container /= Nothing
                 then throwSemanticError ("Duplicate identifier '" ++ sourceName ++ "'.") expression
                 else return ()

                 if isCustomType (readVariableType sourceName) && typeContainer == Nothing
                 then throwSemanticError ("Reference to undeclared type '" ++ isolateCustomTypeTag sourceName ++ "'.") expression
                 else return ()

                 expressionSemantics rightOperand

                 addVariable (Variable {variableName = sourceName,
                                        variableType = (readVariableType sourceName),
                                        variableIsGlobal = True})

            k -> error "3"
     else throwSemanticError ("Attempt to declare a global variable in a local namespace.") expression

localSemantics :: Statement -> CodeTransformation ()
localSemantics Statement {statementContents = expressionList} =
  do mapM_ localVariableDeclarationSemantics expressionList

localVariableDeclarationSemantics :: Statement -> CodeTransformation ()
localVariableDeclarationSemantics Statement {statementContents = (expression:_)} =
  do symbols <- gets semanticStateSymbols
     types <- gets semanticStateTypes
     nameSpace <- gets semanticStateNameSpace
     if nameSpace == NO_NAMESPACE
     then case statementID expression of
            EXPRESSION_IDENTIFIER ->
              do let sourceName = identifierExpressionValue ((head . statementContents) expression)
                     name = removeTypeTag (map toLower sourceName)
                     container = Map.lookup name symbols
                     typeContainer = Map.lookup (map toLower (isolateCustomTypeTag sourceName)) types

                 if container /= Nothing
                 then throwSemanticError ("Duplicate identifier '" ++ sourceName ++ "'.") expression
                 else return ()

                 if isCustomType (readVariableType sourceName) && typeContainer == Nothing
                 then throwSemanticError ("Reference to undeclared type '" ++ isolateCustomTypeTag sourceName ++ "'.") expression
                 else return ()

                 addVariable (Variable {variableName = name,
                                        variableType = (readVariableType sourceName),
                                        variableIsGlobal = False})
            EXPRESSION_ASSIGN ->

              do let (leftOperand,rightOperand) = getBinaryOperands (statementContents expression)
                     sourceName = identifierExpressionValue (getInitialStatement leftOperand)
                     name = removeTypeTag (map toLower sourceName)
                     container = Map.lookup name symbols
                     typeContainer = Map.lookup (map toLower (isolateCustomTypeTag sourceName)) types

                 if container /= Nothing
                 then throwSemanticError ("Duplicate identifier '" ++ sourceName ++ "'.") expression
                 else return ()

                 if isCustomType (readVariableType sourceName) && typeContainer == Nothing
                 then throwSemanticError ("Reference to undeclared type '" ++ isolateCustomTypeTag sourceName ++ "'.") expression
                 else return ()

                 expressionSemantics rightOperand
               
                 addVariable (Variable {variableName = name,
                                        variableType = (readVariableType sourceName),
                                        variableIsGlobal = False})

            k -> error "4"
     else case statementID expression of
            EXPRESSION_IDENTIFIER ->
              do let sourceName = identifierExpressionValue ((head . statementContents) expression)
                     name = removeTypeTag (map toLower sourceName)
                     container = Map.lookup name (functionSymbols nameSpace)
                     offset = (-8) * ((numPreservedRegisters - 1) + functionNumLocals nameSpace + 1)
                     typeContainer = Map.lookup (map toLower (isolateCustomTypeTag sourceName)) types
                 
                 if container /= Nothing
                 then throwSemanticError ("Duplicate identifier '" ++ sourceName ++ "'.") expression
                 else return ()

                 if isCustomType (readVariableType sourceName) && typeContainer == Nothing
                 then throwSemanticError ("Reference to undeclared type '" ++ isolateCustomTypeTag sourceName ++ "'.") expression
                 else return ()

                 addLocalAutomaticVariable
                   (LocalAutomaticVariable {localAutomaticVariableName = name,
                                            localAutomaticVariableType = (readVariableType sourceName),
                                            localAutomaticVariableIsArgument = False,
                                            localAutomaticVariableAddress = offset})

            EXPRESSION_ASSIGN ->
                   
              do let (leftOperand,rightOperand) = getBinaryOperands (statementContents expression)
                     sourceName = identifierExpressionValue (getInitialStatement leftOperand)
                     name = removeTypeTag (map toLower sourceName)
                     container = Map.lookup name (functionSymbols nameSpace)
                     offset = (-8) * ((numPreservedRegisters - 1) + functionNumLocals nameSpace + 1)
                     typeContainer = Map.lookup (map toLower (isolateCustomTypeTag sourceName)) types
                 
                 if container /= Nothing
                 then throwSemanticError ("Duplicate identifier '" ++ sourceName ++ "'.") expression
                 else return ()

                 if isCustomType (readVariableType sourceName) && typeContainer == Nothing
                 then throwSemanticError ("Reference to undeclared type '" ++ isolateCustomTypeTag sourceName ++ "'.") expression
                 else return ()

                 expressionSemantics rightOperand
               
                 addLocalAutomaticVariable
                   (LocalAutomaticVariable {localAutomaticVariableName = name,
                                            localAutomaticVariableType = (readVariableType sourceName),
                                            localAutomaticVariableIsArgument = False,
                                            localAutomaticVariableAddress = offset})
            k -> error "5"

functionReturnSemantics :: Statement -> CodeTransformation ()
functionReturnSemantics (Statement {statementContents = (expression:_)}) =
  do symbols <- gets semanticStateSymbols
     localSymbols <- gets semanticStateLocalSymbols
     types <- gets semanticStateTypes
     nameSpace <- gets semanticStateNameSpace

     if isCustomType (functionType nameSpace)
     then do let expressionTypeName = map toLower (customTypeName (getExpressionType expression symbols localSymbols types))
                 functionTypeName = map toLower (customTypeName (functionType nameSpace))
             if expressionTypeName /= functionTypeName
             then throwSemanticError ("Returned type differs from function's declared type.") expression
             else return ()
     else return ()
     expressionSemantics expression

functionReturnSemantics _ =
  do return ()

#if LINUX==1
extractRegisterArguments :: [(Parameter, Statement)] -> Int -> Int -> Int -> [(Parameter, Statement)]
extractRegisterArguments arguments index numGPRArgumentsTaken numMMRArgumentsTaken

        | index == length arguments = arguments

        | destType (arguments !! index) `elem` [VARIABLE_TYPE_INT, VARIABLE_TYPE_STRING] || isRawRegisterStatement (snd (arguments !! index)) || isCustomType (destType (arguments !! index)) =

                if numGPRArgumentsTaken < numFunctionCallGPRs
                then extractRegisterArguments arguments (index + 1) (numGPRArgumentsTaken + 1) numMMRArgumentsTaken
                else extractRegisterArguments modifiedArguments index numGPRArgumentsTaken numMMRArgumentsTaken

        | destType (arguments !! index) == VARIABLE_TYPE_FLOAT =

                if numMMRArgumentsTaken < numFunctionCallMMRs
                then extractRegisterArguments arguments (index + 1) numGPRArgumentsTaken (numMMRArgumentsTaken + 1)
                else extractRegisterArguments modifiedArguments index numGPRArgumentsTaken numMMRArgumentsTaken

        | True = error (show arguments)

        where destType argument = parameterType (fst argument)
              splitArguments = splitAt (index + 1) arguments
              modifiedArguments = (take ((length (fst splitArguments)) - 1) (fst splitArguments)) ++ snd splitArguments
        
#elif WINDOWS==1
extractRegisterArguments :: [(Parameter, Statement)] -> [(Parameter, Statement)]
extractRegisterArguments arguments =
    take 4 arguments
#endif

isRawRegisterStatement :: Statement -> Bool
isRawRegisterStatement statement =
  case statement of
       RawRegister {} -> True
       _ -> False

#if LINUX==1
extractStackArguments :: [(Parameter, Statement)] -> Int -> Int -> Int -> [(Parameter, Statement)]
extractStackArguments arguments index numGPRArgumentsDropped numMMRArgumentsDropped

        | index == length arguments = arguments

        | destType (arguments !! index) `elem` [VARIABLE_TYPE_INT, VARIABLE_TYPE_STRING] || isRawRegisterStatement (snd (arguments !! index)) || isCustomType (destType (arguments !! index)) =

                if numGPRArgumentsDropped < numFunctionCallGPRs
                then extractStackArguments modifiedArguments index (numGPRArgumentsDropped + 1) numMMRArgumentsDropped
                else extractStackArguments arguments (index + 1) numGPRArgumentsDropped numMMRArgumentsDropped

        | destType (arguments !! index) == VARIABLE_TYPE_FLOAT =

                if numMMRArgumentsDropped < numFunctionCallMMRs
                then extractStackArguments modifiedArguments index numGPRArgumentsDropped (numMMRArgumentsDropped + 1)
                else extractStackArguments arguments (index + 1) numGPRArgumentsDropped numMMRArgumentsDropped
        | True = error (show arguments)

        where destType argument = parameterType (fst argument)
              splitArguments = splitAt (index + 1) arguments
              modifiedArguments = (take ((length (fst splitArguments)) - 1) (fst splitArguments)) ++ snd splitArguments

#elif WINDOWS==1

extractStackArguments :: [(Parameter, Statement)] -> [(Parameter, Statement)]
extractStackArguments arguments =
    drop 4 arguments

#endif

addConst :: Symbol -> CodeTransformation ()
addConst (Const {constName = name,
                 constType = dataType,
                 constValue = value}) =
  do let expression = getInitialStatement (constValueExpression value)
         int = intConstantExpressionValue expression
         float = floatConstantExpressionValue expression
         string = stringConstantExpressionValue expression
     
     state <- get
     put state {semanticStateSymbols = (Map.insert (removeTypeTag (map toLower name)) (Const {constName = name, constType = dataType, constValue = value}) (semanticStateSymbols state))}
     case dataType of
       VARIABLE_TYPE_INT -> addInt int
       VARIABLE_TYPE_FLOAT -> addFloat float
       VARIABLE_TYPE_STRING -> addString string

addVariable :: Symbol -> CodeTransformation ()
addVariable (Variable {variableName = name,
                       variableType = dataType,
                       variableIsGlobal = isGlobal}) =
        do state <- get
           put state {semanticStateSymbols = (Map.insert (removeTypeTag (map toLower name)) (Variable {variableName = name, variableType = dataType, variableIsGlobal = isGlobal}) (semanticStateSymbols state))}

addVariable (Array {arrayName = name,
                    arrayType = dataType,
                    arrayNumDimensions = numDimensions}) =
        do state <- get
           put state {semanticStateSymbols = (Map.insert (removeTypeTag (map toLower name)) (Array {arrayName = name, arrayType = dataType, arrayNumDimensions = numDimensions}) (semanticStateSymbols state))}

addLocalAutomaticVariable :: Symbol -> CodeTransformation ()
addLocalAutomaticVariable symbol =
  do symbols <- gets semanticStateSymbols
     nameSpace <- gets semanticStateNameSpace
     let newFunction = nameSpace {functionNumLocals = functionNumLocals nameSpace + 1,functionSymbols = Map.insert (map toLower (removeTypeTag (localAutomaticVariableName symbol))) symbol (functionSymbols nameSpace)}
         newSymbols = Map.insert (functionRawName newFunction) newFunction symbols
     setSemanticStateNameSpace newFunction
     setSemanticStateLocalSymbols (functionSymbols newFunction)
     setSemanticStateSymbols newSymbols

addLabel :: Symbol -> CodeTransformation ()
addLabel newLabel =
  do symbols <- gets semanticStateSymbols
     localSymbols <- gets semanticStateLocalSymbols
     nameSpace <- gets semanticStateNameSpace
     if nameSpace == NO_NAMESPACE
     then do state <- get
             put state {semanticStateSymbols = (Map.insert (map toLower (labelName newLabel)) newLabel symbols)}
     else do state <- get
             put state {semanticStateLocalSymbols = (Map.insert (map toLower (labelName newLabel)) newLabel localSymbols)}

addFunction :: Symbol -> String -> CodeTransformation ()
addFunction newFunction name =
        do state <- get
           put state {semanticStateSymbols = (Map.insert (map toLower (removeTypeTag name)) newFunction (semanticStateSymbols state))}

addType :: Symbol -> String -> CodeTransformation ()
addType newType name =
  do state <- get
     put state {semanticStateTypes = (Map.insert (map toLower name) newType (semanticStateTypes state))}

addInt :: Int -> CodeTransformation ()
addInt value =
        do state <- get
           let ints = semanticStateInts state
               size = Map.size ints
           if Map.lookup value ints == Nothing
           then put state {semanticStateInts = (Map.insert value size ints)}
           else return ()

addFloat :: Double -> CodeTransformation ()
addFloat value =
        do state <- get
           let floats = semanticStateFloats state
               size = Map.size floats
           if Map.lookup value floats == Nothing
           then put state {semanticStateFloats = (Map.insert value size floats)}
           else return ()

addString :: String -> CodeTransformation ()
addString value =
        do state <- get
           let strings = semanticStateStrings state
               size = Map.size strings
           if Map.lookup value strings == Nothing
           then put state {semanticStateStrings = (Map.insert value (size + 1) strings)}
           else return ()

readVariableType :: String -> VariableType
readVariableType name =
  if '.' `elem` name
  then VARIABLE_TYPE_CUSTOM (isolateCustomTypeTag name)
  else case last name of
            '%' -> VARIABLE_TYPE_INT
            '#' -> VARIABLE_TYPE_FLOAT
            '$' -> VARIABLE_TYPE_STRING
            _   -> VARIABLE_TYPE_INT

readFunctionType :: String -> VariableType
readFunctionType name =
  if '.' `elem` name
  then VARIABLE_TYPE_CUSTOM (isolateCustomTypeTag name)
  else case last name of
            '%' -> VARIABLE_TYPE_INT
            '#' -> VARIABLE_TYPE_FLOAT
            '$' -> VARIABLE_TYPE_STRING
            _   -> VARIABLE_TYPE_VOID

hasTypeTag :: String -> Bool
hasTypeTag name =
  if (last name `elem` "%#$") || ('.' `elem` name)
  then True
  else False

removeTypeTag :: String -> String
removeTypeTag name =
  if '.' `elem` name
  then takeWhile (\c -> c /= '.') name
  else if last name `elem` "%#$"
       then take ((length name) - 1) name
       else name

isolateCustomTypeTag :: String -> String
isolateCustomTypeTag name =
  if isIntrinsicType (readVariableType name)
  then name
  else tail (dropWhile (\c -> c /= '.') name)

isIntrinsicTypeVariable :: Symbol -> Bool
isIntrinsicTypeVariable (Variable {variableType = (VARIABLE_TYPE_CUSTOM {})}) = False
isIntrinsicTypeVariable (Variable {}) = True
isIntrinsicTypeVariable (LocalAutomaticVariable {localAutomaticVariableType = (VARIABLE_TYPE_CUSTOM {})}) = False
isIntrinsicTypeVariable (LocalAutomaticVariable {}) = True
isIntrinsicTypeVariable _ = False

isCustomTypeVariable :: Symbol -> Bool
isCustomTypeVariable (Variable {variableType = (VARIABLE_TYPE_CUSTOM {})}) = True
isCustomTypeVariable (LocalAutomaticVariable {localAutomaticVariableType = (VARIABLE_TYPE_CUSTOM {})}) = True
isCustomTypeVariable _ = False

isIntrinsicType :: VariableType -> Bool
isIntrinsicType dataType = dataType `elem` [VARIABLE_TYPE_INT,VARIABLE_TYPE_FLOAT,VARIABLE_TYPE_STRING]

isCustomType :: VariableType -> Bool
isCustomType (VARIABLE_TYPE_CUSTOM {}) = True
isCustomType VARIABLE_TYPE_NULL = True
isCustomType _ = False

isArray :: Symbol -> Bool
isArray (Array {}) = True
isArray _ = False

isType :: Symbol -> Bool
isType (Type {}) = True
isType _ = False

expressionIsAtomic :: Statement -> Bool

expressionIsAtomic (Statement {statementID = STATEMENT_EXPRESSION, 
                               statementContents = (Statement {statementID = id, 
                                                               statementContents = contents}:_)})
        | id == EXPRESSION_GROUP = expressionIsAtomic (head contents)
        | otherwise = id `elem` atomList

expressionIsAtomic (Statement {statementID = id,
                               statementContents = contents})
        | id == EXPRESSION_GROUP = expressionIsAtomic (head contents)
        | otherwise = id `elem` atomList

expressionIsAtomic _ = True

expressionIsUnary :: Statement -> Bool
expressionIsUnary (Statement {statementID = id}) =
        id `elem` unaryOperatorList

expressionIsBinary :: Statement -> Bool
expressionIsBinary (Statement {statementID = id}) =
        id `elem` binaryOperatorList

expressionIsFunctionCall :: Statement -> Bool
expressionIsFunctionCall (Statement {statementID = STATEMENT_EXPRESSION, 
                                     statementContents = (Statement {statementID = id}:_)}) =
        id == EXPRESSION_FUNCTION_CALL

expressionIsFunctionCall (Statement {statementID = id}) =
        id == EXPRESSION_FUNCTION_CALL

expressionIsFunctionCall _ = False

isStringExpression :: Statement -> SymbolTable -> SymbolTable -> SymbolTable -> Bool
isStringExpression (Statement {statementID = id, 
                               statementContents = contents}) symbols localSymbols types
        | id == EXPRESSION_ADD &&
                  any (== VARIABLE_TYPE_STRING) (map (\ e -> getExpressionType e symbols localSymbols types) contents) = True
        | otherwise = False

isStringExpression st _ _ _ = error ("Critical error in function isStringExpression.\n" ++ show st)

isConstantStringExpression :: Statement -> Bool
isConstantStringExpression (Statement {statementID = id, 
                                       statementContents = contents}) =
        if id == EXPRESSION_STRING_CONSTANT || all isConstantStringExpression contents
        then True
        else False

isConstantStringExpression _ = False

isStringRelationalExpression :: Statement -> SymbolTable -> SymbolTable -> SymbolTable -> Bool
isStringRelationalExpression (Statement {statementID = id, 
                                         statementContents = contents}) symbols localSymbols types
        | id `elem` [EXPRESSION_EQUAL_TO,
                     EXPRESSION_NOT_EQUAL_TO,
                     EXPRESSION_GREATER_THAN,
                     EXPRESSION_LESS_THAN,
                     EXPRESSION_GREATER_THAN_OR_EQUAL_TO,
                     EXPRESSION_LESS_THAN_OR_EQUAL_TO] &&
                     any (== VARIABLE_TYPE_STRING) (map (\ e -> getExpressionType e symbols localSymbols types) contents) = True
        | otherwise = False

getUnaryOperand :: [Statement] -> Statement
getUnaryOperand statements = head statements

getBinaryOperands :: [Statement] -> (Statement, Statement)
getBinaryOperands statements = (head statements, last statements)

getFunctionCallOperands :: [Statement] -> (Statement, [Statement])

getFunctionCallOperands (Statement {statementID = STATEMENT_EXPRESSION, 
                                    statementContents = (Statement {statementID = EXPRESSION_FUNCTION_CALL, 
                                                                    statementContents = statements}:_)}:_) =
        
        (head statements, tail statements)

getFunctionCallOperands statements = (head statements, tail statements)

getInitialStatement :: Statement -> Statement
getInitialStatement = (head . statementContents)

getIdentifierValue :: Statement -> String
getIdentifierValue (Statement {statementID = EXPRESSION_IDENTIFIER, 
                               statementContents = (IdentifierExpression value:_)}) =
        value

getIdentifierValue (Statement {statementID = STATEMENT_EXPRESSION,
                               statementContents = (identifier:_)}) =
        identifierExpressionValue (getInitialStatement identifier)

getIdentifierValue k = error "6"

getIntConstantValue :: Statement -> Int
getIntConstantValue (Statement {statementID = EXPRESSION_INT_CONSTANT, 
                                statementContents = (IntConstantExpression value:_)}) =
        value

getIntConstantValue (Statement {statementID = STATEMENT_EXPRESSION,
                                statementContents = (intConstant:_)}) =
        intConstantExpressionValue (getInitialStatement intConstant)
getIntConstantValue k = error (show k)
getFloatConstantValue :: Statement -> Double
getFloatConstantValue (Statement {statementID = EXPRESSION_FLOAT_CONSTANT, 
                                  statementContents = (FloatConstantExpression value:_)}) =
        value

getFloatConstantValue (Statement {statementID = STATEMENT_EXPRESSION,
                                  statementContents = (floatConstant:_)}) =
        floatConstantExpressionValue (getInitialStatement floatConstant)

getStringConstantValue :: Statement -> String
getStringConstantValue (Statement {statementID = EXPRESSION_STRING_CONSTANT, 
                                   statementContents = (StringConstantExpression value:_)}) =
        value

getStringConstantValue (Statement {statementID = STATEMENT_EXPRESSION,
                                   statementContents = (stringConstant:_)}) =
        stringConstantExpressionValue (getInitialStatement stringConstant)

getHexConstantValue :: Statement -> Int
getHexConstantValue (Statement {statementID = EXPRESSION_HEX_CONSTANT, 
                                   statementContents = (HexConstantExpression value:_)}) =
  hexToInt value

getHexConstantValue (Statement {statementID = STATEMENT_EXPRESSION,
                                   statementContents = (hexConstant:_)}) =
  hexToInt (hexConstantExpressionValue (getInitialStatement hexConstant))

hexToInt :: String -> Int
hexToInt (_:hexConstant) =
  convert (take 16 (reverse hexConstant)) 0
  where convert (digit:digits) place =
          hexDigitToInt digit * 16 ^ place + convert digits (place + 1)
        convert _ _ = 0
          
hexDigitToInt :: Char -> Int
hexDigitToInt digit =
  if digit `elem` "1234567890"
  then read ([digit]) :: Int
  else case toLower digit of
         'a' -> 10
         'b' -> 11
         'c' -> 12
         'd' -> 13
         'e' -> 14
         'f' -> 15

getBinConstantValue :: Statement -> Int
getBinConstantValue (Statement {statementID = EXPRESSION_BIN_CONSTANT, 
                                   statementContents = (BinConstantExpression value:_)}) =
  binToInt value

getBinConstantValue (Statement {statementID = STATEMENT_EXPRESSION,
                                   statementContents = (binConstant:_)}) =
  binToInt (binConstantExpressionValue (getInitialStatement binConstant))

binToInt :: String -> Int
binToInt (_:binConstant) =
  convert (take 64 (reverse binConstant)) 0
  where convert (digit:digits) place =
          binDigitToInt digit * 2 ^ place + convert digits (place + 1)
        convert _ _ = 0

binDigitToInt :: Char -> Int
binDigitToInt digit =
  read ([digit]) :: Int

reduceConstantExpression :: Statement -> SymbolTable -> SymbolTable -> Statement
reduceConstantExpression statement symbols localSymbols =

        case statementID statement of

             STATEMENT_EXPRESSION -> reduceConstantExpression ((head . statementContents) statement) symbols localSymbols
             EXPRESSION_IDENTIFIER ->
               let sourceName = identifierExpressionValue (getInitialStatement statement)
                   name = removeTypeTag (map toLower sourceName)
                   const = fromJust (Map.lookup name symbols)
                   value = constValue const
                   expression = constValueExpression value in
               case constType const of
                 VARIABLE_TYPE_INT ->
                   Statement EXPRESSION_INT_CONSTANT lineNumber offset [IntConstantExpression (getIntConstantValue expression)]
                 VARIABLE_TYPE_FLOAT ->
                   Statement EXPRESSION_FLOAT_CONSTANT lineNumber offset [FloatConstantExpression (getFloatConstantValue expression)]
                 VARIABLE_TYPE_STRING ->
                   Statement EXPRESSION_STRING_CONSTANT lineNumber offset [StringConstantExpression (getStringConstantValue expression)]
                   
             EXPRESSION_INT_CONSTANT -> Statement EXPRESSION_INT_CONSTANT lineNumber offset [IntConstantExpression (getIntConstantValue (statement))]
             EXPRESSION_FLOAT_CONSTANT -> Statement EXPRESSION_FLOAT_CONSTANT lineNumber offset [FloatConstantExpression (getFloatConstantValue (statement))]
             EXPRESSION_PI -> Statement EXPRESSION_FLOAT_CONSTANT lineNumber offset [FloatConstantExpression 3.14159265358979323]
             EXPRESSION_TRUE -> Statement EXPRESSION_INT_CONSTANT lineNumber offset [IntConstantExpression 1]
             EXPRESSION_FALSE -> Statement EXPRESSION_INT_CONSTANT lineNumber offset [IntConstantExpression 0]
             EXPRESSION_STRING_CONSTANT -> Statement EXPRESSION_STRING_CONSTANT lineNumber offset [StringConstantExpression (getStringConstantValue (statement))]
             EXPRESSION_HEX_CONSTANT -> Statement EXPRESSION_INT_CONSTANT lineNumber offset [IntConstantExpression (getHexConstantValue (statement))]
             EXPRESSION_BIN_CONSTANT -> Statement EXPRESSION_INT_CONSTANT lineNumber offset [IntConstantExpression (getBinConstantValue (statement))]
             EXPRESSION_NULL -> Statement EXPRESSION_NULL lineNumber offset [NullExpression]

             EXPRESSION_OR -> Statement EXPRESSION_INT_CONSTANT lineNumber offset [IntConstantExpression (leftInt .|. rightInt)]
             EXPRESSION_AND -> Statement EXPRESSION_INT_CONSTANT lineNumber offset [IntConstantExpression (leftInt .&. rightInt)]

             EXPRESSION_SHL -> Statement EXPRESSION_INT_CONSTANT lineNumber offset [IntConstantExpression (leftInt `shiftL` rightInt)]
             EXPRESSION_SHR -> Statement EXPRESSION_INT_CONSTANT lineNumber offset [IntConstantExpression (leftInt `nonArithmeticShiftR` rightInt)]
             EXPRESSION_SAR -> Statement EXPRESSION_INT_CONSTANT lineNumber offset [IntConstantExpression (leftInt `shiftR` rightInt)]

             EXPRESSION_BITWISE_COMPLEMENT -> Statement EXPRESSION_INT_CONSTANT lineNumber offset [IntConstantExpression (complement int)]

             EXPRESSION_EQUAL_TO -> Statement EXPRESSION_INT_CONSTANT lineNumber offset [IntConstantExpression (boolToInt (leftFloat == rightFloat))]

             EXPRESSION_NOT_EQUAL_TO -> Statement EXPRESSION_INT_CONSTANT lineNumber offset [IntConstantExpression (boolToInt (leftFloat /= rightFloat))]
             EXPRESSION_GREATER_THAN -> Statement EXPRESSION_INT_CONSTANT lineNumber offset [IntConstantExpression (boolToInt (leftFloat > rightFloat))]
             EXPRESSION_LESS_THAN -> Statement EXPRESSION_INT_CONSTANT lineNumber offset [IntConstantExpression (boolToInt (leftFloat < rightFloat))]
             EXPRESSION_GREATER_THAN_OR_EQUAL_TO -> Statement EXPRESSION_INT_CONSTANT lineNumber offset [IntConstantExpression (boolToInt (leftFloat >= rightFloat))]
             EXPRESSION_LESS_THAN_OR_EQUAL_TO -> Statement EXPRESSION_INT_CONSTANT lineNumber offset [IntConstantExpression (boolToInt (leftFloat <= rightFloat))]

             EXPRESSION_NOT -> Statement EXPRESSION_INT_CONSTANT lineNumber offset [IntConstantExpression (boolToInt (not (int /= 0)))]

             EXPRESSION_GROUP -> reduceConstantExpression unaryOperand symbols localSymbols

             EXPRESSION_INT ->
               case unaryType of
                 VARIABLE_TYPE_INT -> Statement EXPRESSION_INT_CONSTANT lineNumber offset [IntConstantExpression int]
                 VARIABLE_TYPE_FLOAT -> Statement EXPRESSION_INT_CONSTANT lineNumber offset [IntConstantExpression (round float)]
                 VARIABLE_TYPE_STRING -> Statement EXPRESSION_INT_CONSTANT lineNumber offset [IntConstantExpression (read (removeQuotes string) :: Int)]
             EXPRESSION_FLOAT ->
               case unaryType of
                 VARIABLE_TYPE_INT -> Statement EXPRESSION_FLOAT_CONSTANT lineNumber offset [FloatConstantExpression (fromIntegral int)]
                 VARIABLE_TYPE_FLOAT -> Statement EXPRESSION_FLOAT_CONSTANT lineNumber offset [FloatConstantExpression float]
                 VARIABLE_TYPE_STRING -> Statement EXPRESSION_FLOAT_CONSTANT lineNumber offset [FloatConstantExpression (read (removeQuotes string) :: Double)]
             EXPRESSION_STR ->
               case unaryType of
                 VARIABLE_TYPE_INT -> Statement EXPRESSION_STRING_CONSTANT lineNumber offset [StringConstantExpression ("\"" ++ (show int) ++ "\"")]
                 VARIABLE_TYPE_FLOAT -> Statement EXPRESSION_STRING_CONSTANT lineNumber offset [StringConstantExpression ("\"" ++ (show float) ++ "\"")]
                 VARIABLE_TYPE_STRING -> Statement EXPRESSION_STRING_CONSTANT lineNumber offset [StringConstantExpression string]
                 
             _ -> if statementID statement `elem`
                     [EXPRESSION_ADD,EXPRESSION_SUB,EXPRESSION_MUL,EXPRESSION_DIV,EXPRESSION_MOD,EXPRESSION_POW]
                  then case getBinaryArithmeticExpressionType statement symbols localSymbols Map.empty of
                         VARIABLE_TYPE_STRING -> Statement EXPRESSION_STRING_CONSTANT lineNumber offset [StringConstantExpression ("\"" ++ removeQuotes leftString ++ removeQuotes rightString ++ "\"")]
                         VARIABLE_TYPE_FLOAT -> Statement EXPRESSION_FLOAT_CONSTANT lineNumber offset [FloatConstantExpression (floatArithmeticOperator leftFloat rightFloat)]
                         VARIABLE_TYPE_INT -> Statement EXPRESSION_INT_CONSTANT lineNumber offset [IntConstantExpression (intArithmeticOperator leftInt rightInt)]
                  else case statementID statement of
                        EXPRESSION_NEG ->
                          case getUnaryArithmeticExpressionType statement symbols localSymbols Map.empty of
                              VARIABLE_TYPE_INT -> Statement EXPRESSION_INT_CONSTANT lineNumber offset [IntConstantExpression (-int)]
                              VARIABLE_TYPE_FLOAT -> Statement EXPRESSION_FLOAT_CONSTANT lineNumber offset [FloatConstantExpression (-float)]
                        EXPRESSION_POS ->
                          case getUnaryArithmeticExpressionType statement symbols localSymbols Map.empty of
                              VARIABLE_TYPE_INT -> Statement EXPRESSION_INT_CONSTANT lineNumber offset [IntConstantExpression (abs int)]
                              VARIABLE_TYPE_FLOAT -> Statement EXPRESSION_FLOAT_CONSTANT lineNumber offset [FloatConstantExpression (abs float)]
                        _ ->  error ("Critical error in function reduceConstantExpression.\n" ++ (show statement))

             where (leftOperand, rightOperand) = getBinaryOperands (statementContents statement)
                   unaryOperand = getUnaryOperand (statementContents statement)                         

                   leftInt = getIntConstantValue (reductionTypeFilter (reduceConstantExpression leftOperand symbols localSymbols) VARIABLE_TYPE_INT leftType)
                   rightInt = getIntConstantValue (reductionTypeFilter (reduceConstantExpression rightOperand symbols localSymbols) VARIABLE_TYPE_INT rightType)
                   leftFloat = getFloatConstantValue (reductionTypeFilter (reduceConstantExpression leftOperand symbols localSymbols) VARIABLE_TYPE_FLOAT leftType)
                   rightFloat = getFloatConstantValue (reductionTypeFilter (reduceConstantExpression rightOperand symbols localSymbols) VARIABLE_TYPE_FLOAT rightType)

                   leftString = getStringConstantValue (reductionTypeFilter (reduceConstantExpression leftOperand symbols localSymbols) VARIABLE_TYPE_STRING leftType)
                   rightString = getStringConstantValue (reductionTypeFilter (reduceConstantExpression rightOperand symbols localSymbols) VARIABLE_TYPE_STRING rightType)

                   int = getIntConstantValue (reductionTypeFilter (reduceConstantExpression unaryOperand symbols localSymbols) VARIABLE_TYPE_INT unaryType)
                   float = getFloatConstantValue (reductionTypeFilter (reduceConstantExpression unaryOperand symbols localSymbols) VARIABLE_TYPE_FLOAT unaryType)
                   string = getStringConstantValue (reductionTypeFilter (reduceConstantExpression unaryOperand symbols localSymbols) VARIABLE_TYPE_STRING unaryType)

                   (leftType, rightType) = (getExpressionType leftOperand symbols localSymbols Map.empty,
                                            getExpressionType rightOperand symbols localSymbols Map.empty)

                   unaryType = getExpressionType unaryOperand symbols localSymbols Map.empty

                   intArithmeticOperator a b =
                     case statementID statement of
                       EXPRESSION_ADD -> (+) a b
                       EXPRESSION_SUB -> (-) a b
                       EXPRESSION_MUL -> (*) a b
                       EXPRESSION_DIV -> (div a b)
                       EXPRESSION_MOD -> (mod a b)
                       EXPRESSION_POW -> (^) a b

                   floatArithmeticOperator a b =
                     case statementID statement of
                       EXPRESSION_ADD -> (+) a b
                       EXPRESSION_SUB -> (-) a b
                       EXPRESSION_MUL -> (*) a b
                       EXPRESSION_DIV -> (/) a b
                       EXPRESSION_MOD -> (mod' a b)
                       EXPRESSION_POW -> (**) a b
                       
                   boolToInt bool | bool == True = 1
                                  | otherwise = 0
                   
                   lineNumber = statementLineNumber statement
                   offset = if expressionIsBinary statement
                            then statementOffset (getInitialStatement statement)   
                            else statementOffset statement     

nonArithmeticShiftR :: Int -> Int -> Int
nonArithmeticShiftR l r =
  clearMostSignificantBits (l `shiftR` r) r
  where clearMostSignificantBits value n =
          if n < 0
          then value
#if LINUX==1
          else clearMostSignificantBits (clearBit value ((bitSize value) - n)) (n - 1)
#elif WINDOWS==1
          else clearMostSignificantBits (clearBit value ((fromJust (bitSizeMaybe value)) - n)) (n - 1)
#endif

reductionTypeFilter :: Statement -> VariableType -> VariableType -> Statement
reductionTypeFilter statement destType sourceType
        | destType == sourceType = statement
        | otherwise =

          case types of

               (VARIABLE_TYPE_INT, VARIABLE_TYPE_FLOAT) ->

                        Statement EXPRESSION_INT_CONSTANT lineNumber offset [IntConstantExpression (floor $ getFloatConstantValue statement)]

               (VARIABLE_TYPE_INT, VARIABLE_TYPE_STRING) ->

                        Statement EXPRESSION_INT_CONSTANT lineNumber offset [IntConstantExpression (read (removeQuotes (getStringConstantValue statement)) :: Int)]

               (VARIABLE_TYPE_FLOAT, VARIABLE_TYPE_INT) ->

                        Statement EXPRESSION_FLOAT_CONSTANT lineNumber offset [FloatConstantExpression (fromIntegral $ getIntConstantValue statement)]
                        
               (VARIABLE_TYPE_FLOAT, VARIABLE_TYPE_STRING) ->

                        Statement EXPRESSION_FLOAT_CONSTANT lineNumber offset [FloatConstantExpression (read (getStringConstantValue statement) :: Double)]
               (VARIABLE_TYPE_STRING, VARIABLE_TYPE_INT) ->

                        Statement EXPRESSION_STRING_CONSTANT lineNumber offset [StringConstantExpression ("\"" ++ (show (getIntConstantValue statement)) ++ "\"")]

               (VARIABLE_TYPE_STRING, VARIABLE_TYPE_FLOAT) ->

                        Statement EXPRESSION_STRING_CONSTANT lineNumber offset [StringConstantExpression ("\"" ++ (show (getFloatConstantValue statement)) ++ "\"")]
               (VARIABLE_TYPE_CUSTOM {}, VARIABLE_TYPE_CUSTOM {}) ->
                        Statement EXPRESSION_INT_CONSTANT lineNumber offset [IntConstantExpression 1]

               (k,l) -> error "7"
                        
          where types = (destType, sourceType)
                lineNumber = statementLineNumber statement
                offset = statementOffset statement

getExpressionType :: Statement -> SymbolTable -> SymbolTable -> SymbolTable -> VariableType
getExpressionType (Statement {statementID = STATEMENT_EXPRESSION, 
                              statementContents = (expression:_)}) symbols localSymbols types =
  getExpressionType expression symbols localSymbols types

getExpressionType (RawRegister _ dataType) symbols localSymbols types =
  dataType

getExpressionType expression symbols localSymbols types

  | statementID expression == EXPRESSION_ASSIGN =
    let (leftOperand, rightOperand) = getBinaryOperands (statementContents expression) in
    getExpressionType leftOperand symbols localSymbols types
  | statementID expression `elem`
    [EXPRESSION_OR,
     EXPRESSION_AND,
     EXPRESSION_BITWISE_COMPLEMENT,
     EXPRESSION_SHL,
     EXPRESSION_SHR,
     EXPRESSION_SAR,
     EXPRESSION_NOT,
     EXPRESSION_EQUAL_TO,
     EXPRESSION_NOT_EQUAL_TO,
     EXPRESSION_GREATER_THAN,
     EXPRESSION_LESS_THAN,
     EXPRESSION_GREATER_THAN_OR_EQUAL_TO,
     EXPRESSION_LESS_THAN_OR_EQUAL_TO] = VARIABLE_TYPE_INT
  | statementID expression `elem`
    [EXPRESSION_ADD,
     EXPRESSION_SUB,
     EXPRESSION_MUL,
     EXPRESSION_DIV,
     EXPRESSION_MOD,
     EXPRESSION_POW] =
      getBinaryArithmeticExpressionType expression symbols localSymbols types
  | statementID expression `elem`
    [EXPRESSION_POS,
     EXPRESSION_NEG] =
      getUnaryArithmeticExpressionType expression symbols localSymbols types
  | statementID expression ==
    EXPRESSION_GROUP =
      getExpressionType (getInitialStatement expression) symbols localSymbols types
  | statementID expression ==
    EXPRESSION_INT =
      VARIABLE_TYPE_INT
  | statementID expression ==
    EXPRESSION_FLOAT =
      VARIABLE_TYPE_FLOAT
  | statementID expression ==
    EXPRESSION_STR =
      VARIABLE_TYPE_STRING
  | statementID expression ==
    EXPRESSION_FUNCTION_CALL =
      let sourceName = getIdentifierValue (getInitialStatement expression)
          name = removeTypeTag (map toLower sourceName)
          symbolContainer = Map.lookup name symbols
          symbol = fromJust symbolContainer in

      if symbolContainer /= Nothing
      then case symbol of
             (Function {functionType = dataType}) ->
               dataType
             (Array {arrayType = VARIABLE_TYPE_ARRAY {targetType = dataType}}) ->
               dataType
      else readFunctionType sourceName
  | statementID expression ==
    EXPRESSION_NEW =
      let operand = (getInitialStatement . getInitialStatement) expression in
          VARIABLE_TYPE_CUSTOM (identifierExpressionValue operand)
  | statementID expression ==
    EXPRESSION_FIRST =
      let operand = (getInitialStatement . getInitialStatement) expression in
          VARIABLE_TYPE_CUSTOM (identifierExpressionValue operand)
  | statementID expression ==
    EXPRESSION_LAST =
      let operand = (getInitialStatement . getInitialStatement) expression in
          VARIABLE_TYPE_CUSTOM (identifierExpressionValue operand)
  | statementID expression ==
    EXPRESSION_BEFORE =
      let operand = getInitialStatement expression in
          getExpressionType operand symbols localSymbols types
  | statementID expression ==
    EXPRESSION_AFTER =
      let operand = getInitialStatement expression in
          getExpressionType operand symbols localSymbols types
  | statementID expression ==
    EXPRESSION_FIELD_ACCESS =
      let (leftOperand, rightOperand) = getBinaryOperands (statementContents expression)
          (VARIABLE_TYPE_CUSTOM {customTypeName = sourceName}) = getExpressionType leftOperand symbols localSymbols types
          name = map toLower sourceName
          typeContainer = Map.lookup name types
          type_ = fromJust typeContainer
          fieldSourceName = identifierExpressionValue (getInitialStatement rightOperand)
          fieldName = map toLower (removeTypeTag fieldSourceName)
          fieldContainer = Map.lookup fieldName (typeSymbols type_)
          field = fromJust fieldContainer
          in if typeContainer == Nothing || fieldContainer == Nothing
             then VARIABLE_TYPE_CUSTOM fieldName
             else fieldType field

  | statementID expression ==
    EXPRESSION_IDENTIFIER =
      let sourceName = getIdentifierValue expression
          name = map toLower (removeTypeTag sourceName)
          symbolContainer =
            let globalSymbolContainer = Map.lookup name symbols
                localSymbolContainer = Map.lookup name localSymbols in
            if localSymbolContainer /= Nothing
            then localSymbolContainer
            else globalSymbolContainer
          symbol = fromJust symbolContainer in

      if symbolContainer == Nothing
      then readVariableType sourceName
      else case symbol of
             (Const {constType = dataType}) ->
               dataType
             (Variable {variableType = dataType}) ->
               dataType
             (LocalAutomaticVariable {localAutomaticVariableType = dataType}) ->
               dataType
             (Array {arrayType = VARIABLE_TYPE_ARRAY {targetType = dataType}}) ->
               dataType
             (Function {functionType = dataType}) ->
               dataType
             k -> error (show k)
  | statementID expression `elem` [EXPRESSION_INT_CONSTANT,EXPRESSION_HEX_CONSTANT,EXPRESSION_BIN_CONSTANT] =
      VARIABLE_TYPE_INT
  | statementID expression `elem` [EXPRESSION_FLOAT_CONSTANT,EXPRESSION_PI] =
      VARIABLE_TYPE_FLOAT
  | statementID expression ==
    EXPRESSION_STRING_CONSTANT =
      VARIABLE_TYPE_STRING
  | statementID expression ==
    EXPRESSION_NULL =
      VARIABLE_TYPE_CUSTOM {customTypeName = "Null"}
  | statementID expression ==
    EXPRESSION_TRUE = VARIABLE_TYPE_INT
  | statementID expression ==
    EXPRESSION_FALSE = VARIABLE_TYPE_INT
  | otherwise = error ("Critical error in function getExpressionType.\n" ++ (show expression))

getBinaryArithmeticExpressionType :: Statement -> SymbolTable -> SymbolTable -> SymbolTable -> VariableType
getBinaryArithmeticExpressionType expression symbols localSymbols types =
  let (leftOperand, rightOperand) = getBinaryOperands (statementContents expression)
      leftOperandType = getExpressionType leftOperand symbols localSymbols types
      rightOperandType = getExpressionType rightOperand symbols localSymbols types in
  if VARIABLE_TYPE_STRING `elem` [leftOperandType,rightOperandType]
  then VARIABLE_TYPE_STRING
  else if VARIABLE_TYPE_FLOAT `elem` [leftOperandType,rightOperandType]
       then VARIABLE_TYPE_FLOAT
       else VARIABLE_TYPE_INT

getUnaryArithmeticExpressionType :: Statement -> SymbolTable -> SymbolTable -> SymbolTable -> VariableType
getUnaryArithmeticExpressionType expression symbols localSymbols types =
  let operand = getUnaryOperand (statementContents expression)
      operandType = getExpressionType operand symbols localSymbols types in
  if operandType == VARIABLE_TYPE_STRING
  then VARIABLE_TYPE_STRING
  else if operandType == VARIABLE_TYPE_FLOAT
       then VARIABLE_TYPE_FLOAT
       else VARIABLE_TYPE_INT

getRelationalExpressionOperandType :: Statement -> SymbolTable -> SymbolTable -> SymbolTable -> VariableType
getRelationalExpressionOperandType expression symbols localSymbols types =
  let (leftOperand, rightOperand) = getBinaryOperands (statementContents expression)
      leftOperandType = getExpressionType leftOperand symbols localSymbols types
      rightOperandType = getExpressionType rightOperand symbols localSymbols types in
  if isCustomType leftOperandType && isCustomType rightOperandType
  then if customTypeName leftOperandType == "Null"
       then VARIABLE_TYPE_CUSTOM {customTypeName = customTypeName rightOperandType}
       else VARIABLE_TYPE_CUSTOM {customTypeName = customTypeName leftOperandType}
  else if VARIABLE_TYPE_STRING `elem` [leftOperandType,rightOperandType]
       then VARIABLE_TYPE_STRING
       else if VARIABLE_TYPE_FLOAT `elem` [leftOperandType,rightOperandType]
            then VARIABLE_TYPE_FLOAT
            else VARIABLE_TYPE_INT

expressionIsConstant :: SymbolTable -> Statement -> Bool
expressionIsConstant globalSymbols (Statement {statementID = id,statementContents = contents}) =
  case id of
    EXPRESSION_ASSIGN -> False
    EXPRESSION_IDENTIFIER ->
      let sourceName = identifierExpressionValue (head contents)
          name = removeTypeTag (map toLower sourceName)
          symbolContainer = Map.lookup name globalSymbols in
          if symbolContainer == Nothing
          then False
          else case fromJust symbolContainer of
                 (Const {}) -> True
                 _ -> False
    EXPRESSION_TRUE -> True
    EXPRESSION_FALSE -> True
    EXPRESSION_NULL -> True
    EXPRESSION_INT_CONSTANT -> True
    EXPRESSION_FLOAT_CONSTANT -> True
    EXPRESSION_PI -> True
    EXPRESSION_STRING_CONSTANT -> True
    EXPRESSION_BIN_CONSTANT -> True
    EXPRESSION_HEX_CONSTANT -> True
    EXPRESSION_INT -> expressionIsConstant globalSymbols (head contents)
    EXPRESSION_FLOAT -> expressionIsConstant globalSymbols (head contents)
    EXPRESSION_STR -> expressionIsConstant globalSymbols (head contents)
    _ -> all (expressionIsConstant globalSymbols) contents

expressionIsConstant globalSymbols (IdentifierExpression sourceName) =
  let name = removeTypeTag (map toLower sourceName)
      symbolContainer = Map.lookup name globalSymbols in
      if symbolContainer == Nothing
      then False
      else case fromJust symbolContainer of
             (Const {}) -> True
             _ -> False

expressionIsConstant _ _ = True

expressionIsLabel :: Statement -> Bool
expressionIsLabel (Statement {statementID = STATEMENT_EXPRESSION,statementContents=(expression:_)}) =
  expressionIsLabel expression

expressionIsLabel (Statement {statementID = EXPRESSION_IDENTIFIER,statementContents=(identifierExpression:_)}) =
  head (identifierExpressionValue identifierExpression) == '.'
  
expressionIsLabel _ = False

lookupVariable :: String -> SymbolTable -> SymbolTable -> Symbol -> Symbol
lookupVariable sourceName globalSymbols localSymbols nameSpace =

        if symbol == Nothing
        then error ("Critical error: symbol lookup failed on symbol '" ++ sourceName ++ "'.")
        else fromJust symbol

        where name = removeTypeTag (map toLower sourceName)
              symbol =
                let global = (Map.lookup name globalSymbols)
                    local = (Map.lookup name localSymbols) in

                    if local /= Nothing
                    then local
                    else if global /= Nothing
                         then if nameSpace == NO_NAMESPACE
                              then global
                              else if isConst (fromJust global) || isGlobal (fromJust global) || isArray (fromJust global) || isFunc (fromJust global)
                                   then global
                                   else Nothing
                         else Nothing

isGlobal :: Symbol -> Bool
isGlobal symbol =
  case symbol of
    Variable {variableIsGlobal = isGlobal} -> isGlobal
    _ -> False

isConst :: Symbol -> Bool
isConst symbol =
  case symbol of
    Const {} -> True
    _ -> False

isFunc :: Symbol -> Bool
isFunc symbol =
  case symbol of
    Function {} -> True
    _ -> False

lookupType :: String -> SymbolTable -> Symbol
lookupType sourceName types =
  let name = map toLower sourceName
      type_ = fromJust (Map.lookup name types) in
        type_

lookupField :: String -> SymbolTable -> Symbol
lookupField sourceName fields =
  let name = removeTypeTag (map toLower sourceName)
      field = fromJust (Map.lookup name fields) in
        field

lookupInt :: Int -> IntConstantTable -> Int
lookupInt value intTable =

        if container == Nothing
        then error ("Critical error: integer table lookup failed on integer '" ++ show value ++ "'.")
        else fromJust container
        where container = Map.lookup value intTable

lookupFloat :: Double -> FloatConstantTable -> Int
lookupFloat value floatTable =

        if container == Nothing
        then error ("Critical error: float table lookup failed on float '" ++ show value ++ "'.")
        else fromJust container
        where container = Map.lookup value floatTable

lookupString :: String -> StringTable -> Int
lookupString value stringTable =

        if container == Nothing
        then error ("Critical error: string table lookup failed on string '" ++ value ++ "'. This should never happen.")
        else fromJust container
        where container = Map.lookup value stringTable

setSemanticStateLocalSymbols :: SymbolTable -> CodeTransformation ()
setSemanticStateLocalSymbols symbolTable =

        do state <- get
           put state {semanticStateLocalSymbols = symbolTable}

createLibraryFunction :: String -> VariableType -> [Parameter] -> Symbol
createLibraryFunction name returnType parameterList =

        Function name name returnType FUNCTION_ORIGIN_SYSTEM parameterList 0 0 0 0 0 False Map.empty

createStandardFunction :: String -> VariableType -> [Parameter] -> Symbol
createStandardFunction name returnType parameterList =

        Function (decorateStandardFunctionName name) name returnType FUNCTION_ORIGIN_STANDARD parameterList 0 0 0 0 0 False Map.empty

decorateStandardFunctionName :: String -> String
decorateStandardFunctionName name = "bb_" ++ name

standardFunctionIsUsed :: String -> SymbolTable -> Bool
standardFunctionIsUsed sourceName symbols =
  do let name = map toLower (removeTypeTag sourceName)
         functionContainer = Map.lookup name symbols
         function = fromJust functionContainer in
       functionUsed function

markStandardFunctionAsUsed :: String -> CodeTransformation ()
markStandardFunctionAsUsed sourceName =
  do state <- get
     symbols <- gets semanticStateSymbols
     let name = map toLower (removeTypeTag sourceName)
         functionContainer = Map.lookup name symbols
         function = fromJust functionContainer
         updatedFunction = function {functionUsed = True}
         updatedSymbols = Map.adjust (\f -> updatedFunction) name symbols
     put (state {semanticStateSymbols = updatedSymbols})

createUserFunction :: String -> VariableType -> [Parameter] -> Int -> Int -> Int -> Int -> Int-> SymbolTable -> Symbol
createUserFunction name returnType parameterList maxNumArguments minNumArguments numRegisterArguments numStackArguments numLocals symbolTable =

        Function (decorateUserFunctionName name) name returnType FUNCTION_ORIGIN_USER parameterList maxNumArguments minNumArguments numRegisterArguments numStackArguments numLocals False symbolTable
                                            
decorateUserFunctionName :: String -> String
decorateUserFunctionName name = "bbu_" ++ name

createCPUContext :: Int -> CPUContext
createCPUContext offset =
  CPUContext NO_REGISTER
             []
             numFPRs
             allRegisters
             offset
             False
             VARIABLE_TYPE_VOID
             []

createAsm :: Asm
createAsm =

  Asm code_ (Map.fromList [(id_, createAsmBucket),
                           (directives_, createAsmBucket),
                           (globals_, createAsmBucket),
                           (code_, createAsmBucket),
                           (functions_, createAsmBucket),
                           (data_, createAsmBucket)])

createAsmBucket :: AsmBucket
createAsmBucket = AsmBucket Seq.empty

setSemanticStateNameSpace :: Symbol -> CodeTransformation ()
setSemanticStateNameSpace symbol =
  do state <- get
     put state {semanticStateNameSpace = symbol}

setSemanticStateSymbols :: SymbolTable -> CodeTransformation ()
setSemanticStateSymbols symbolTable =
  do state <- get
     put state {semanticStateSymbols = symbolTable}

throwSemanticError :: String -> Statement -> CodeTransformation ()
throwSemanticError msg (Statement {statementLineNumber = lineNumber,
                                   statementOffset = offset}) =
  do state <- get
     let fileName = head (semanticStateIncludeFileNameStack state)
     throwError (FatalError fileName lineNumber offset msg)
