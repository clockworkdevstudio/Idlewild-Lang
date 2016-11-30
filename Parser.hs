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

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Parser where

import LexerData
import ParserData
import SemanticsData

import StandardFunctions
import Common
import Options

import System.IO
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.Trans.Maybe
import Debug.Trace

import Data.List
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Foldable as Foldable

createStatement :: StatementID -> Token -> [Statement] -> Statement
createStatement id token contents = Statement id (tokenLineNumber token) (tokenOffset token) contents

statementLocation :: Statement -> String
statementLocation statement = (show (statementLineNumber statement)) ++ ":" ++ (show (statementOffset statement))

parse :: CodeTransformation ()
parse = 
  do verbose <- gets (optionVerbose . configOptions . parseStateConfig)
     verboseCommentary ("Parsing...\n") verbose
     parseProgram
     program <- gets parseStateTree
     config <- gets parseStateConfig
     includeFileNameStack <- gets parseStateIncludeFileNameStack
     
     let options = configOptions config 

     if optionAbstractSyntaxTree options
     then do liftIO $ putStr (printAbstractSyntaxTree 0 program)
     else return ()
     put SemanticState
           {semanticStateIncludeFileNameStack = includeFileNameStack,
            semanticStateProgram = [program],
            semanticStateSymbols = idlewildLangStandardFunctions,
            semanticStateLocalSymbols =  Map.empty,
            semanticStateTypes = Map.empty,
            semanticStateInts = Map.empty,
            semanticStateFloats = Map.empty,
            semanticStateStrings = Map.empty,
            semanticStateNameSpace = NO_NAMESPACE,
            semanticStateLoopDepth = 0,
            semanticStateConfig = config}
     

parserPushIncludeFileName :: String -> CodeTransformation ()
parserPushIncludeFileName fileName =
  do state <- get
     put $ state {parseStateIncludeFileNameStack =
                  (fileName:parseStateIncludeFileNameStack state)}

parserPopIncludeFileName :: CodeTransformation ()
parserPopIncludeFileName =
  do state <- get
     put $ state {parseStateIncludeFileNameStack =
                  tail (parseStateIncludeFileNameStack state)}

lookToken :: CodeTransformation Token
lookToken =
  do (token:_) <- gets parseStateTokens
     return token

popToken :: CodeTransformation Token
popToken =
  do (token:_) <- gets parseStateTokens
     dropToken
     return token

parseProgram :: CodeTransformation ()
parseProgram =
        do token <- lookToken
           statement <- parseCode Seq.empty True
           state <- get
           put state {parseStateTree = createStatement STATEMENT_PROGRAM token [statement]}
           return ()

parseCode :: Seq.Seq Statement -> Bool -> CodeTransformation Statement
parseCode statements acceptNewlines =

        do token <- lookToken
           if endOfCode token
           then do return (createMinimalStatement STATEMENT_CODE (Foldable.toList statements))
           else do statement <- (parseFunc token)
                   parseCode (statements Seq.|> statement) acceptNewlines

             where parseFunc token =
                     case tokenID token of
                          TOKEN_INCLUDE_FILE_NAME -> doNothing
                          TOKEN_BEGINNING_OF_FILE -> parseBeginningOfFile
                          TOKEN_END_OF_FILE -> parseEndOfFile
                          TOKEN_NEWLINE -> parseNewLine
                          TOKEN_COLON -> parseEndStatement
                          TOKEN_IDENTIFIER -> parseTopLevelExpression
                          TOKEN_ASM -> parseAsm STATEMENT_ASM
                          TOKEN_ASM_DATA -> parseAsm STATEMENT_ASM_DATA
                          TOKEN_SYS -> parseSys
                          TOKEN_DIM -> parseDim
                          TOKEN_LABEL -> parseLabel
                          TOKEN_IF -> parseIf
                          TOKEN_SELECT -> parseSelect
                          TOKEN_FOR -> parseFor
                          TOKEN_WHILE -> parseWhile
                          TOKEN_REPEAT -> parseRepeat
                          TOKEN_EXIT -> parseExit
                          TOKEN_ON -> parseOn
                          TOKEN_GOTO -> parseGoto
                          TOKEN_GOSUB -> parseGosub
                          TOKEN_RETURN -> parseReturn
                          TOKEN_DATA -> parseData
                          TOKEN_READ -> parseRead
                          TOKEN_RESTORE -> parseRestore
                          TOKEN_FUNCTION -> parseMultiFunction
                          TOKEN_CONST -> parseConst
                          TOKEN_GLOBAL -> parseGlobal
                          TOKEN_LOCAL -> parseLocal
                          TOKEN_TYPE -> parseType
                          TOKEN_INSERT -> parseInsert
                          TOKEN_DELETE -> parseDelete
                          TOKEN_DELETE_EACH -> parseDeleteEach
                          TOKEN_END -> parseTopLevelExpression
                          _ -> throwParseError ("Unexpected token '" ++ tokenValue token ++ "'.") token

                   endOfCode token =
                     if tokenID token `elem` endOfBlockTokens || (tokenID token == TOKEN_NEWLINE && not acceptNewlines)
                     then True
                     else False

                   doNothing = do dropToken
                                  return (createMinimalStatement STATEMENT_NONE [EmptyStatement])

parseNewLine :: CodeTransformation Statement
parseNewLine =
  do token <- popToken
     return (createStatement STATEMENT_NEWLINE token [])

parseBeginningOfFile :: CodeTransformation Statement
parseBeginningOfFile =
  do token <- popToken
     parserPushIncludeFileName (tokenValue token)
     return (createStatement STATEMENT_BEGINNING_OF_FILE token [IdentifierExpression (tokenValue token)])

parseEndOfFile :: CodeTransformation Statement
parseEndOfFile =
  do token <- popToken
     parserPopIncludeFileName
     return (createStatement STATEMENT_END_OF_FILE token [IdentifierExpression (tokenValue token)])

parseEndStatement :: CodeTransformation Statement
parseEndStatement =
        do token  <- popToken
           return (createStatement STATEMENT_END_STATEMENT token [])

parseAsm :: StatementID -> CodeTransformation Statement
parseAsm id =
  do initToken <- popToken
     asm <- consumeAsm []
     return (createStatement id initToken [InlineAsm asm])
     
consumeAsm :: [String] -> CodeTransformation [String]
consumeAsm asm =
  do token <- popToken
     if tokenID token == TOKEN_END_ASM
     then return asm
     else consumeAsm (asm ++ [tokenValue token])

parseSys :: CodeTransformation Statement
parseSys =
        do initToken <- popToken
           name <- requireCode (parseExpressionUntil TOKEN_LEFT_PARENTHESIS [EXPRESSION_IDENTIFIER]) "Expecting identifier."
           requireCode (expectToken TOKEN_LEFT_PARENTHESIS) "Expecting left parenthesis."
           token <- lookToken
           parameterList <- parseExpressionList [] [EXPRESSION_IDENTIFIER,EXPRESSION_EQUAL_TO] token TOKEN_RIGHT_PARENTHESIS True
           requireCode (expectToken TOKEN_RIGHT_PARENTHESIS) "Expecting right parenthesis."
           let prototype = (createStatement STATEMENT_FUNCTION_PROTOTYPE token parameterList)
           return (createStatement STATEMENT_SYS initToken [name, prototype])

parseDim :: CodeTransformation Statement
parseDim =
        do token <- popToken
           expressionList <- parseExpressionList [] [EXPRESSION_FUNCTION_CALL] token TOKEN_NEWLINE False
           return (createStatement STATEMENT_DIM token expressionList)

parseLabel :: CodeTransformation Statement
parseLabel =
        do token <- popToken
           dropTokensWhile (\ t -> tokenID t `elem` meaninglessTokens)
           nextToken <- lookToken
           if tokenID nextToken == TOKEN_DATA
           then return (createStatement STATEMENT_DATA_LABEL token [IdentifierExpression (tokenValue token)])
           else return (createStatement STATEMENT_LABEL token [IdentifierExpression (tokenValue token)])

parseOn :: CodeTransformation Statement
parseOn =
  do initToken <- popToken
     predicate <- requireCode (parseExpression []) "Expecting expression."
     token <- popToken
     destinationList <- parseExpressionList [] [EXPRESSION_IDENTIFIER] token TOKEN_NEWLINE False
     case tokenID token of
          
          TOKEN_GOTO ->
            
            return (createStatement STATEMENT_ON_GOTO initToken (predicate:destinationList))

          TOKEN_GOSUB ->  
            
            return (createStatement STATEMENT_ON_GOSUB initToken (predicate:destinationList))


parseGoto :: CodeTransformation Statement
parseGoto =
        do initToken <- popToken
           token <- lookToken
           requireCode (expectToken TOKEN_IDENTIFIER) "Expecting identifier."
           return (createStatement STATEMENT_GOTO initToken [(createStatement EXPRESSION_IDENTIFIER token [IdentifierExpression (tokenValue token)])])

parseGosub :: CodeTransformation Statement
parseGosub =
        do initToken <- popToken
           token <- lookToken
           requireCode (expectToken TOKEN_IDENTIFIER) "Expecting identifier."
           return (createStatement STATEMENT_GOSUB initToken [(createStatement EXPRESSION_IDENTIFIER token [IdentifierExpression (tokenValue token)])])

parseReturn :: CodeTransformation Statement
parseReturn =
  do inFunction <- gets parseStateInFunction
     if inFunction
     then do token <- popToken
             value <- anticipateCode (expression 0 [TOKEN_NEWLINE,TOKEN_COLON])
             if value == EmptyStatement
             then return (createStatement STATEMENT_MULTI_FUNCTION_RETURN token [])
             else return (createStatement STATEMENT_MULTI_FUNCTION_RETURN token [value])
     else do token <- popToken
             return (createStatement STATEMENT_RETURN token [])

parseIf :: CodeTransformation Statement
parseIf =

  do initToken <- popToken
     predicateStatement <- requireCode (parseExpression []) "Expecting expression."
     bridgeToken <- popToken
     consequenceToken <- lookToken
     if tokenID bridgeToken == TOKEN_NEWLINE || (tokenID bridgeToken == TOKEN_THEN && tokenID consequenceToken == TOKEN_NEWLINE)
     then do code <- anticipateCode (parseCode Seq.empty True)
             elseIfStatements <- parseElseIfStatements [createStatement STATEMENT_IF_DEFAULT initToken [predicateStatement,code]]
             requireCode (expectToken TOKEN_END_IF) "Expecting 'End If'."
             return (createStatement STATEMENT_IF initToken (elseIfStatements))
     else do case tokenID bridgeToken of
                  TOKEN_THEN ->
                    
                    do contents <- requireCode (parseCode Seq.empty False) "Expecting statement."
                       return (createStatement STATEMENT_IF initToken [createStatement STATEMENT_IF_DEFAULT initToken [predicateStatement,contents]])

                  TOKEN_GOTO ->

                    do token <- popToken
                       let destinationStatement = (createStatement STATEMENT_LABEL token [IdentifierExpression (tokenValue token)])
                       return (createStatement STATEMENT_IF initToken [predicateStatement,destinationStatement])

parseElseIfStatements :: [Statement] -> CodeTransformation [Statement]
parseElseIfStatements elseIfStatements =
    do initToken <- lookToken
       case tokenID initToken of
            TOKEN_ELSE_IF ->
              do dropToken
                 predicateExpression <- requireCode (parseExpression []) "Expecting expression."
                 code <- anticipateCode (parseCode Seq.empty True)
                 parseElseIfStatements (elseIfStatements ++ [createStatement STATEMENT_ELSE_IF initToken [predicateExpression,code]])
            TOKEN_ELSE ->
              do dropToken
                 code <- anticipateCode (parseCode Seq.empty True)
                 return (elseIfStatements ++ [createStatement STATEMENT_ELSE initToken [code]])
            TOKEN_NEWLINE ->
              do dropToken
                 parseElseIfStatements elseIfStatements
            _ -> return elseIfStatements

parseSelect :: CodeTransformation Statement
parseSelect =

    do initToken <- popToken
       selectExpression <- requireCode (parseExpression []) "Expecting expression."
       dropTokensWhile (\t -> tokenID t == TOKEN_NEWLINE)
       caseStatements <- parseCaseStatements []
       requireCode (expectToken TOKEN_END_SELECT) "Expecting 'End Select'."
       return (createStatement STATEMENT_SELECT initToken (selectExpression:caseStatements))

parseCaseStatements :: [Statement] -> CodeTransformation [Statement]
parseCaseStatements caseStatements =
    do initToken <- lookToken
       case tokenID initToken of
            TOKEN_CASE ->
              do dropToken
                 caseExpression <- requireCode (parseExpression []) "Expecting expression."
                 code <- anticipateCode (parseCode Seq.empty True)
                 parseCaseStatements (caseStatements ++ [createStatement STATEMENT_CASE initToken [caseExpression,code]])
            TOKEN_DEFAULT ->
              do dropToken
                 code <- anticipateCode (parseCode Seq.empty True)
                 return (caseStatements ++ [createStatement STATEMENT_DEFAULT initToken [code]])
            TOKEN_NEWLINE ->
              do dropToken
                 parseCaseStatements caseStatements
            _ -> return caseStatements

parseForHeader :: CodeTransformation Statement
parseForHeader =

        do initToken <- popToken           
           token <- lookToken
           l <- requireCode (expression 0 [TOKEN_EQUAL_TO]) "Expecting expression."
           requireCode (expectToken TOKEN_EQUAL_TO) "Expecting assignment expression." 
           each <- anticipateCode (expectToken TOKEN_EACH)
           if parserSuccess each
           then do r <- requireCode (parseExpression [EXPRESSION_IDENTIFIER]) "Expecting expression."
                   let initialiserStatement = createStatement STATEMENT_EXPRESSION token [createStatement EXPRESSION_ASSIGN token [l,r]]
                   return (createStatement STATEMENT_FOR_EACH_HEADER initToken [initialiserStatement])
           else do r <- requireCode (expression 0 []) "Expecting assignment expression."
                   let initialiserStatement = createStatement STATEMENT_EXPRESSION token [createStatement EXPRESSION_ASSIGN token [l,r]]

                   requireCode (expectToken TOKEN_TO) "Expecting 'To'."
                   limitStatement <- requireCode (parseExpression []) "Expecting expression."

                   token <- lookToken

                   if tokenID token == TOKEN_STEP
                   then do dropToken
                           stepStatement <- requireCode (parseExpression []) "Expecting expression."
                           return (createStatement STATEMENT_FOR_HEADER initToken [initialiserStatement, limitStatement, stepStatement])
                   else do return (createStatement STATEMENT_FOR_HEADER initToken [initialiserStatement, limitStatement])

parseFor :: CodeTransformation Statement
parseFor =

        do token <- lookToken
           forHeaderStatement <- requireCode parseForHeader []
           code <- requireCode (parseCode Seq.empty True) []
           requireCode (expectToken TOKEN_NEXT) "Expecting 'Next <identifier>'."
           if statementID forHeaderStatement == STATEMENT_FOR_EACH_HEADER
           then return (createStatement STATEMENT_FOR_EACH token [forHeaderStatement, code])
           else return (createStatement STATEMENT_FOR token [forHeaderStatement, code])

parseWhile :: CodeTransformation Statement
parseWhile =

        do token <- popToken
           predicateExpression <- requireCode (parseExpression []) "Expecting expression."
           code <- anticipateCode (parseCode Seq.empty True)
           requireCode (expectToken TOKEN_WEND) "Expecting 'Wend'."
           return (createStatement STATEMENT_WHILE token [predicateExpression,code])

parseRepeat :: CodeTransformation Statement
parseRepeat =

  do repeatToken <- popToken
     code <- anticipateCode (parseCode Seq.empty True)
     token <- popToken
     case tokenID token of
          TOKEN_UNTIL ->
            do predicateExpression <- requireCode (parseExpression []) "Expecting expression."
               let until = createStatement STATEMENT_UNTIL token [predicateExpression]
               return (createStatement STATEMENT_REPEAT repeatToken [code,until])
          TOKEN_FOREVER ->
            do let forever = createStatement STATEMENT_FOREVER token []
               return (createStatement STATEMENT_REPEAT repeatToken [code,forever])
          _ -> throwParseError "Expecting 'Until' or 'Forever'." token

parseExit :: CodeTransformation Statement
parseExit =

  do exitToken <- popToken
     return (createStatement STATEMENT_EXIT exitToken [])
                          
parseData :: CodeTransformation Statement
parseData =
        do initToken <- popToken
           token <- lookToken
           expressionList <- parseExpressionList [] [] token TOKEN_NEWLINE False
           return (createStatement STATEMENT_DATA initToken expressionList)

parseRead :: CodeTransformation Statement
parseRead =
        do initToken <- popToken
           token <- lookToken
           variableList <- parseExpressionList [] [EXPRESSION_IDENTIFIER,EXPRESSION_FIELD_ACCESS,EXPRESSION_FUNCTION_CALL] token TOKEN_NEWLINE False
           return (createStatement STATEMENT_READ token variableList)

parseRestore :: CodeTransformation Statement
parseRestore =
        do initToken <- popToken
           token <- lookToken
           requireCode (expectToken TOKEN_IDENTIFIER) "Expecting label."
           return (createStatement STATEMENT_RESTORE initToken [createStatement STATEMENT_LABEL token  [IdentifierExpression (tokenValue token)]])

parseMultiFunction :: CodeTransformation Statement
parseMultiFunction =
  do st <- get
     put st {parseStateInFunction = True}
     initToken <- popToken
     name <- requireCode (parseExpressionUntil TOKEN_LEFT_PARENTHESIS [EXPRESSION_IDENTIFIER]) "Expecting identifier."
     requireCode (expectToken TOKEN_LEFT_PARENTHESIS) "Expecting left parenthesis."
     token <- lookToken
     parameterList <- parseExpressionList [] [EXPRESSION_IDENTIFIER,EXPRESSION_EQUAL_TO] token TOKEN_RIGHT_PARENTHESIS True
     requireCode (expectToken TOKEN_RIGHT_PARENTHESIS) "Expecting right parenthesis."
     definition <- anticipateCode (parseCode Seq.empty True)
     requireCode (expectToken TOKEN_END_FUNCTION) "Expecting 'End Function'."
     st2 <- get
     put st2 {parseStateInFunction = False}
     let prototype = (createStatement STATEMENT_FUNCTION_PROTOTYPE token parameterList)
     return (createStatement STATEMENT_MULTI_FUNCTION initToken [name, prototype, definition])

parseConst :: CodeTransformation Statement
parseConst =
  do initToken <- popToken
     variableDeclarations <- parseVariableDeclarationList [] initToken
     return (createStatement STATEMENT_CONST initToken variableDeclarations)

parseGlobal :: CodeTransformation Statement
parseGlobal =
  do initToken <- popToken
     variableDeclarations <- parseVariableDeclarationList [] initToken
     return (createStatement STATEMENT_GLOBAL initToken variableDeclarations)

parseLocal :: CodeTransformation Statement
parseLocal =
  do initToken <- popToken
     variableDeclarations <- parseVariableDeclarationList [] initToken
     return (createStatement STATEMENT_LOCAL initToken variableDeclarations)
                             
parseType :: CodeTransformation Statement
parseType =
  do initToken <- popToken
     typeName <- requireCode (parseExpression [EXPRESSION_IDENTIFIER]) "Expecting identifier."
     fieldDeclarations <- parseFieldDeclarations [typeName]
     return (createStatement STATEMENT_TYPE initToken fieldDeclarations)
     
parseFieldDeclarations :: [Statement] -> CodeTransformation [Statement]
parseFieldDeclarations originalFields =
  do dropTokensWhile (\t -> tokenID t == TOKEN_NEWLINE)
     token <- popToken
     case tokenID token of
          TOKEN_FIELD ->
            do fields <- parseExpressionList [] [EXPRESSION_IDENTIFIER] token TOKEN_NEWLINE False
               parseFieldDeclarations (originalFields ++ fields)
          TOKEN_END_TYPE ->
            do return originalFields
          _ ->
            do _ <- throwParseError "Expecting 'Field' or 'End Type'." token
               return []

parseInsert :: CodeTransformation Statement
parseInsert =
  do token <- popToken
     object <- requireCode (parseExpression []) "Expecting expression."
     before <- anticipateCode (expectToken TOKEN_BEFORE)
     if parserSuccess before
     then do expression <- requireCode (parseExpression []) "Expecting expression."
             return (createStatement STATEMENT_INSERT_BEFORE token [object,expression])
     else do after <- anticipateCode (expectToken TOKEN_AFTER) 
             if parserSuccess after
             then do expression <- requireCode (parseExpression []) "Expecting expression."
                     return (createStatement STATEMENT_INSERT_AFTER token [object,expression])
             else throwParseError "Expecting 'Before' or 'After'." token

parseDelete :: CodeTransformation Statement
parseDelete =
  do token <- popToken
     object <- requireCode (parseExpression []) "Expecting expression."
     return (createStatement STATEMENT_DELETE token [object])

parseDeleteEach :: CodeTransformation Statement
parseDeleteEach =
  do token <- popToken
     typeName <- requireCode (parseExpression [EXPRESSION_IDENTIFIER]) "Expecting expression."
     return (createStatement STATEMENT_DELETE_EACH token [typeName])

parseEnd :: CodeTransformation Statement
parseEnd =
  
  do initToken <- popToken
     token <- lookToken
     let functionName = (createStatement EXPRESSION_IDENTIFIER initToken [IdentifierExpression "end"]) 
     if tokenID token == TOKEN_INT_CONSTANT
     then do let exitCode = (createStatement EXPRESSION_INT_CONSTANT token [IntConstantExpression (read (tokenValue token) :: Int)])
             dropToken
             return (createStatement STATEMENT_EXPRESSION initToken [createStatement EXPRESSION_FUNCTION_CALL initToken [functionName,exitCode]])   
     else return (createStatement STATEMENT_EXPRESSION initToken [createStatement EXPRESSION_FUNCTION_CALL initToken [functionName]])
     
parseExpressionList :: [Statement] -> [StatementID] -> Token -> TokenID -> Bool -> CodeTransformation [Statement]
parseExpressionList expressions ids token delimiter optional =

        do expression <- anticipateCode (parseOptionalExpression ids delimiter)
           if expression == EmptyStatement
           then if not optional
                then do _ <- throwParseError "Expecting expression." token
                        return []
                else do return []
           else do comma <- anticipateCode (expectToken TOKEN_COMMA)
                   if parserSuccess comma
                   then do parseExpressionList (expressions ++ [expression]) ids token delimiter optional
                   else return (expressions ++ [expression])

parseExpression :: [StatementID] -> CodeTransformation Statement
parseExpression ids =

        do token <- lookToken
           expression <- requireCode (expression 0 []) "Expecting expression."
           if not (null ids || (statementID expression `elem` ids))
           then do token <- lookToken
                   throwParseError "Illegal expression." token
           else do return (createStatement STATEMENT_EXPRESSION token [expression])

parseOptionalExpression :: [StatementID] -> TokenID -> CodeTransformation Statement
parseOptionalExpression ids delimiter =

        do token <- lookToken
           expression <- anticipateCode (expression 0 [delimiter])  
           if expression == EmptyStatement
           then return EmptyStatement
           else if not (null ids || (statementID expression `elem` ids))
                then do token <- lookToken
                        throwParseError "Illegal expression." token
                else do return (createStatement STATEMENT_EXPRESSION token [expression])

parseExpressionUntil :: TokenID -> [StatementID] -> CodeTransformation Statement
parseExpressionUntil delimiter ids =

        do token <- lookToken
           expression <- requireCode (expression 0 [delimiter]) "Expecting expression."
           if not (null ids || (statementID expression `elem` ids))
           then do token <- lookToken
                   throwParseError "Illegal expression." token
           else do return (createStatement STATEMENT_EXPRESSION token [expression])

parseVariableDeclaration :: CodeTransformation Statement
parseVariableDeclaration =
  do token <- lookToken
     l <- requireCode (expression 0 [TOKEN_EQUAL_TO]) "Expecting expression."
     if statementID l /= EXPRESSION_IDENTIFIER
     then throwParseError "Illegal expression." token
     else do e <- anticipateCode (expectToken TOKEN_EQUAL_TO)
             if parserSuccess e
             then do r <- requireCode (expression 0 []) "Expecting assignment expression."
                     return (createStatement STATEMENT_EXPRESSION token [createStatement EXPRESSION_ASSIGN token [l,r]])    
             else return (createStatement STATEMENT_EXPRESSION token [createStatement EXPRESSION_IDENTIFIER token [IdentifierExpression (tokenValue token)]])    

parseVariableDeclarationList :: [Statement] -> Token ->  CodeTransformation [Statement]
parseVariableDeclarationList expressions token =

  do expression <- requireCode parseVariableDeclaration "Expecting comma separated variable declaration list."
     comma <- anticipateCode (expectToken TOKEN_COMMA)
     if parserSuccess comma
     then do parseVariableDeclarationList (expressions ++ [expression]) token
     else return (expressions ++ [expression])   

parseLet :: CodeTransformation Statement
parseLet =
        do dropToken
           parseTopLevelExpression

parseTopLevelExpression :: CodeTransformation Statement
parseTopLevelExpression =
        do initToken <- lookToken
           l <- requireCode (expression 0 (TOKEN_EQUAL_TO:nastyTokenHack)) "Expecting expression."
           if not (statementID l `elem` writableExpressionTypes)
           then do throwParseError "Illegal top-level expression." initToken
           else return EmptyStatement
           token <- lookToken
           case tokenID token of
                TOKEN_EQUAL_TO ->
                  do dropToken
                     r <- requireCode (expression 0 []) "Expecting assignment expression."
                     return (createStatement STATEMENT_EXPRESSION initToken [createStatement EXPRESSION_ASSIGN initToken  [l,r]])
                TOKEN_LEFT_PARENTHESIS ->
                  do f <- ambiguousArgumentList initToken l
                     t1 <- lookToken
                     if tokenID t1 == TOKEN_EQUAL_TO
                     then do dropToken
                             k <- expression 0 []
                             let a = (createStatement EXPRESSION_ASSIGN initToken [f,k])
                                 b = (createStatement STATEMENT_EXPRESSION initToken [a])
                             return b
                     else do r <- expressionLoop 0 f []
                             let b = (createStatement STATEMENT_EXPRESSION initToken [r])
                             return b
                _ -> if statementID l `elem` standaloneExpressionTypes
                     then return (createStatement STATEMENT_EXPRESSION initToken [l])
                     else if statementID l == EXPRESSION_IDENTIFIER || statementID l == STATEMENT_END
                          then do f <- argumentList (createStatement EXPRESSION_FUNCTION_CALL token [l]) [TOKEN_NEWLINE,TOKEN_COLON]
                                  return (createStatement STATEMENT_EXPRESSION initToken [f])
                          else throwParseError "Illegal top-level expression." initToken

getOperatorFromToken :: Token -> [Operator] -> Operator
getOperatorFromToken (Token {tokenID = id}) opsList =

  do let ct = find (\ o -> operatorTokenID o == id) opsList
     if ct == Nothing
     then Operator TOKEN_NONE STATEMENT_EXPRESSION (-1) ASSOCIATIVITY_LEFT
     else fromJust ct

expression :: Int -> [TokenID] -> CodeTransformation Statement
expression depth delimiters =

  do token <- lookToken
     if tokenID token `elem` delimiters
     then return EmptyStatement
     else do let op = getOperatorFromToken token prefixOperators
             if operatorExpressionID op == EXPRESSION_GROUP
             then do g <- groupExpression
                     expressionLoop depth g []
             else if operatorAssociativity op == ASSOCIATIVITY_RIGHT && operatorPrecedence op >= 0
                  then do dropToken
                          o <- expression ((operatorPrecedence op) + 1) []
                          expressionLoop depth (createStatement (operatorExpressionID op) token [o]) []
                  else do a <- requireCode atom "Expecting expression."
                          expressionLoop depth a delimiters

expressionLoop :: Int -> Statement -> [TokenID] -> CodeTransformation Statement
expressionLoop depth lhs delimiters =

  do token <- lookToken
     if tokenID token `elem` delimiters
     then return lhs
     else do let op = getOperatorFromToken token infixOperators
             if operatorExpressionID op == EXPRESSION_FUNCTION_CALL
             then do if operatorPrecedence op < depth
                     then do return lhs
                     else do
                             if statementID lhs `elem` typeConversionOperatorList
                             then do token <- popToken
                                     o <- requireCode (expression 0 [TOKEN_RIGHT_PARENTHESIS]) ("Expecting expression.")
                                     requireCode (expectToken TOKEN_RIGHT_PARENTHESIS) "Expecting right parenthesis."
                                     expressionLoop depth (createStatement (statementID lhs) token [o]) delimiters
                             else do token <- popToken
                                     f <- argumentList (createStatement EXPRESSION_FUNCTION_CALL token [lhs]) [TOKEN_RIGHT_PARENTHESIS]
                                     requireCode (expectToken TOKEN_RIGHT_PARENTHESIS) "Expecting right parenthesis."
                                     expressionLoop depth f delimiters
              else if statementID lhs `elem` typeConversionOperatorList && statementContents lhs == []
                   then throwParseError "Expecting left parenthesis." token
                   else if (operatorAssociativity op == ASSOCIATIVITY_LEFT && operatorPrecedence op > depth) || (operatorAssociativity op == ASSOCIATIVITY_RIGHT && operatorPrecedence op >= depth)
                        then do dropToken
                                rhs <- expression (operatorPrecedence op) delimiters
                                token <- lookToken
                                expressionLoop depth (createStatement (operatorExpressionID op) token [lhs, rhs]) delimiters
                        else do return lhs

groupExpression :: CodeTransformation Statement
groupExpression =

  do token <- popToken
     c <- expression 0 []
     requireCode (expectToken TOKEN_RIGHT_PARENTHESIS) "Expecting right parenthesis.3"
     return (createStatement EXPRESSION_GROUP token [c])

partialArgument :: CodeTransformation Statement
partialArgument =

  do token <- popToken
     c <- expression 0 [TOKEN_RIGHT_PARENTHESIS]
     return c

argumentList :: Statement -> [TokenID] -> CodeTransformation Statement
argumentList (Statement {statementID = EXPRESSION_FUNCTION_CALL,
                         statementLineNumber = lineNumber,
                         statementOffset = offset,
                         statementContents = arguments}) delimiters =

 do argument <- anticipateCode (expression 0 delimiters)
    if parserSuccess argument
    then do comma <- anticipateCode (expectToken TOKEN_COMMA)
            if parserSuccess comma
            then do result <- requireCode (argumentList (Statement EXPRESSION_FUNCTION_CALL lineNumber offset (arguments ++ [argument])) delimiters) "Expecting expression."
                           
                    return result
            else do return (Statement EXPRESSION_FUNCTION_CALL lineNumber offset (arguments ++ [argument]))

    else do return (Statement EXPRESSION_FUNCTION_CALL lineNumber offset arguments)

ambiguousArgumentList :: Token -> Statement -> CodeTransformation Statement
ambiguousArgumentList token l =
  do g <- partialArgument
     t1 <- popToken
     case tokenID t1 of
       TOKEN_COMMA ->
         do f <- argumentList (createStatement EXPRESSION_FUNCTION_CALL token [l,g]) [TOKEN_NEWLINE,TOKEN_COLON]
            requireCode (expectToken TOKEN_RIGHT_PARENTHESIS) "Expecting right parenthesis."
            return f
       TOKEN_RIGHT_PARENTHESIS ->
         do t2 <- lookToken
            if tokenID t2 `elem` [TOKEN_NEWLINE,TOKEN_COLON,TOKEN_END_OF_FILE]
            then do if g == EmptyStatement
                    then do let f = createStatement EXPRESSION_FUNCTION_CALL token [l]
                            return f
                    else do let f = createStatement EXPRESSION_FUNCTION_CALL token [l,g]
                            return f
            else do let j = createStatement EXPRESSION_FUNCTION_CALL token [l,g]
                    case tokenID t2 of
                      TOKEN_COMMA ->
                        do dropToken
                           m <- argumentList j []
                           return m
                      TOKEN_EQUAL_TO ->
                        do dropToken
                           k <- expression 0 [TOKEN_COMMA,TOKEN_NEWLINE,TOKEN_COLON,TOKEN_END_OF_FILE]
                           let stmt = (createStatement EXPRESSION_ASSIGN token [j,k])
                           return stmt
                      _ ->
                        do k <- expressionLoop 0 g [TOKEN_COMMA,TOKEN_NEWLINE,TOKEN_COLON,TOKEN_END_OF_FILE]
                           t3 <- lookToken
                           if tokenID t3 `elem` [TOKEN_NEWLINE,TOKEN_COLON,TOKEN_END_OF_FILE]
                           then do return  (createStatement EXPRESSION_FUNCTION_CALL token [l,k])
                           else do m <- argumentList (createStatement EXPRESSION_FUNCTION_CALL token [l,k]) []
                                   return m
                    
       _ -> throwParseError ("Expecting comma or right parenthesis.") t1

atom :: CodeTransformation Statement
atom =
  do token <- lookToken
     case tokenID token of
          TOKEN_IDENTIFIER ->

            do dropToken
               return (createStatement EXPRESSION_IDENTIFIER token [IdentifierExpression (tokenValue token)])

          TOKEN_END ->

            do dropToken
               return (createStatement EXPRESSION_IDENTIFIER token [IdentifierExpression (tokenValue token)])

          TOKEN_LABEL ->

            do dropToken
               return (createStatement EXPRESSION_IDENTIFIER token [IdentifierExpression (tokenValue token)])


          TOKEN_INT_CONSTANT ->

            do dropToken
               return (createStatement EXPRESSION_INT_CONSTANT token [IntConstantExpression (read (tokenValue token) :: Int)])

          TOKEN_FLOAT_CONSTANT ->

            do dropToken
               return (createStatement EXPRESSION_FLOAT_CONSTANT token [FloatConstantExpression (read (tokenValue token) :: Double)])

          TOKEN_STRING_CONSTANT ->

            do dropToken
               return (createStatement EXPRESSION_STRING_CONSTANT token [StringConstantExpression (tokenValue token)])

          TOKEN_HEX_CONSTANT ->

            do dropToken
               return (createStatement EXPRESSION_HEX_CONSTANT token [HexConstantExpression (tokenValue token)])

          TOKEN_BIN_CONSTANT ->

            do dropToken
               return (createStatement EXPRESSION_BIN_CONSTANT token [BinConstantExpression (tokenValue token)])

          TOKEN_PI ->

            do dropToken
               return (createStatement EXPRESSION_PI token [PiExpression])

          TOKEN_TRUE ->

            do dropToken
               return (createStatement EXPRESSION_TRUE token [TrueExpression])

          TOKEN_FALSE ->

            do dropToken
               return (createStatement EXPRESSION_FALSE token [FalseExpression])

          TOKEN_NULL ->

            do dropToken
               return (createStatement EXPRESSION_NULL token [NullExpression])

          TOKEN_INT ->

            do dropToken
               return (createStatement EXPRESSION_INT token [])

          TOKEN_FLOAT ->

            do dropToken
               return (createStatement EXPRESSION_FLOAT token [])

          TOKEN_STR ->

            do dropToken
               return (createStatement EXPRESSION_STR token [])


          _ -> do return EmptyStatement

anticipateCode :: CodeTransformation Statement -> CodeTransformation Statement
anticipateCode action =

  do result <- action
     return result

requireCode :: CodeTransformation Statement -> String -> CodeTransformation Statement
requireCode action msg =

  do result <- action
     if result == EmptyStatement
     then do token <- lookToken
             throwParseError msg token
     else return result

parserSuccess :: Statement -> Bool
parserSuccess statement = statement /= EmptyStatement

expectToken :: TokenID -> CodeTransformation Statement
expectToken id =
        do token <- lookToken
           if tokenID token == id
           then do dropToken
                   return STATEMENT_TOKEN
           else do return EmptyStatement

expectTokens :: [TokenID] -> CodeTransformation Statement
expectTokens (id : rest) =
        do token <- lookToken
           if tokenID token == id
           then do dropToken
                   expectTokens rest
           else do return EmptyStatement

expectTokens _ =
        do return STATEMENT_TOKEN

dropToken :: CodeTransformation ()
dropToken =
        do state <- get
           put state {parseStateTokens = tail (parseStateTokens state)}

dropTokensWhile :: (Token -> Bool) -> CodeTransformation ()
dropTokensWhile predicate =
        do token <- lookToken
           if predicate token
           then do dropToken
                   dropTokensWhile predicate
           else return ()

printAbstractSyntaxTree :: Int -> Statement -> String
printAbstractSyntaxTree depth (Statement {statementID = id,
                               statementContents = contents}) =
        if null contents
        then indentation ++ stmt ++ "\n"
        else indentation ++ stmt ++ "\n" ++ concat (map (printAbstractSyntaxTree (depth + 1)) contents)

        where indentation = take (depth * 4) (repeat ' ')
              stmt = show id
              
printAbstractSyntaxTree depth (IdentifierExpression value) =
        indentation ++ (show value) ++ "\n"
        where indentation = take (depth * 4) (repeat ' ')

printAbstractSyntaxTree depth (IntConstantExpression value) =
        indentation ++ (show value) ++ "\n"
        where indentation = take (depth * 4) (repeat ' ')

printAbstractSyntaxTree depth (FloatConstantExpression value) =
        indentation ++ (show value) ++ "\n"
        where indentation = take (depth * 4) (repeat ' ')

printAbstractSyntaxTree depth (StringConstantExpression value) =
        indentation ++ (show value) ++ "\n"
        where indentation = take (depth * 4) (repeat ' ')

printAbstractSyntaxTree depth (HexConstantExpression value) =
        indentation ++ (show value) ++ "\n"
        where indentation = take (depth * 4) (repeat ' ')

printAbstractSyntaxTree depth (BinConstantExpression value) =
        indentation ++ (show value) ++ "\n"
        where indentation = take (depth * 4) (repeat ' ')

printAbstractSyntaxTree depth PiExpression =
        indentation ++ (show 3.14159265358979323) ++ "\n"
        where indentation = take (depth * 4) (repeat ' ')

printAbstractSyntaxTree depth NullExpression =
        indentation ++ "Null" ++ "\n"
        where indentation = take (depth * 4) (repeat ' ')

printAbstractSyntaxTree depth TrueExpression =
        indentation ++ "True" ++ "\n"
        where indentation = take (depth * 4) (repeat ' ')

printAbstractSyntaxTree depth FalseExpression =
        indentation ++ "False" ++ "\n"
        where indentation = take (depth * 4) (repeat ' ')

printAbstractSyntaxTree depth EmptyStatement =
        indentation ++ "EMPTY_STATEMENT" ++ "\n"
        where indentation = take (depth * 4) (repeat ' ')

printAbstractSyntaxTree depth (InlineAsm asm) =
        (concatMap (formatting) asm) ++ "\n"
        where indentation = take (depth * 4) (repeat ' ')
              formatting s = if s == "\n"
                             then s ++ indentation
                             else s
                                  
throwParseError :: String -> Token -> CodeTransformation Statement
throwParseError msg (Token {tokenOffset = offset,
                            tokenLineNumber = lineNumber}) =
  do state <- get
     let fileName = head (parseStateIncludeFileNameStack state)
     throwError (FatalError fileName lineNumber offset msg)
