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

module Lexer where

import LexerData
import ParserData
import Common
import Options

import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Identity

import System.IO

import Data.List
import Data.Function
import qualified Data.Sequence as Seq
import qualified Data.Foldable as Foldable
import qualified Data.Map as Map
import Data.Char
import Data.Maybe
import Debug.Trace

prettyToken :: Token -> String
prettyToken token =
  show (tokenID token) ++ " " ++ show (tokenValue token) ++ "\n"

tokenise :: CodeTransformation ()
tokenise =
  do verbose <- gets (optionVerbose . configOptions . lexStateConfig)
     verboseCommentary ("Scanning...\n") verbose
     tokeniseCode

tokeniseCode :: CodeTransformation ()
tokeniseCode =
    do done <- gets lexDone
       if done
       then do depth <- gets lexStateIncludeFileDepth
               stack <- gets lexStateIncludeFileNameStack
               if depth == 0
               then do tokens <- gets lexStateTokens
                       config <- gets lexStateConfig
                       lineNumber <- gets lexStateLineNumber
                       offset <- gets lexStateLineOffset
                       let sentinel = Token TOKEN_NONE "" lineNumber offset
                           
       
                       put ParseState {parseStateIncludeFileNameStack = stack,
                                       parseStateTree = EmptyStatement,
                                       parseStateTokens = (Foldable.toList (tokens Seq.|> sentinel)),
                                       parseStateInFunction = False,
                                       parseStateConfig = config}
               
               else return ()
               
       else do lineNumber <- gets lexStateLineNumber
               lineOffset <- gets lexStateLineOffset
               pendingTokens <- gets lexStateTokens
               
               if not (Seq.null pendingTokens)
               then do let (_ Seq.:> item) = Seq.viewr pendingTokens
                       
                       case tokenID item of
                         TOKEN_INCLUDE_FILE_NAME ->
                           do cachedState <- get
                              let config = lexStateConfig cachedState
                                  fileNameString = drop (length "Include") (tokenValue item)
                                  sourceFileName = removeQuotes fileNameString
                                  tokenBOF = createBOFToken sourceFileName
                                  tokenEOF = createEOFToken sourceFileName
                              fromHandle <- liftIO $ openFile sourceFileName ReadMode
                              code <- liftIO $  hGetContents fromHandle
                              put LexState
                                {lexStateID = LEX_PENDING,
                                 lexStateIncludeFileDepth = lexStateIncludeFileDepth cachedState + 1,
                                 lexStateIncludeFileNameStack = sourceFileName:(lexStateIncludeFileNameStack cachedState),
                                 lexStateCurrentToken = emptyToken,
                                 lexStatePendingTokens = Seq.empty,
                                 lexStateTokens = Seq.singleton tokenBOF,
                                 lexStateLineNumber = 1,
                                 lexStateLineOffset = 0,
                                 lexStateCharacters = code,
                                 lexStateCompoundTokens = allCompoundTokens,
                                 lexStateConfig = Config
                                                    {configInputFile = fromHandle,
                                                     configOutputFile = configOutputFile config,
                                                     configSourceFileName = sourceFileName,
                                                     configAsmFileName = configAsmFileName config,
                                                     configObjectFileName = configObjectFileName config,
                                                     configOptions = configOptions config}}
                              tokeniseCode
                              newState <- get
                              put cachedState {lexStateTokens = (lexStateTokens cachedState Seq.>< lexStateTokens newState) Seq.|> tokenEOF,
                                               lexStateIncludeFileDepth = (lexStateIncludeFileDepth newState) - 1,
                                               lexStateIncludeFileNameStack = (lexStateIncludeFileNameStack cachedState)}

                              liftIO $ hClose fromHandle

                         TOKEN_ASM ->
                           do tokeniseAsmCode
                         TOKEN_ASM_DATA ->
                           do tokeniseAsmCode
                         _ ->  return ()
               else return ()

               characters <- gets lexStateCharacters
               match characters lineNumber lineOffset
               resetLexState
               tokeniseCode

    where match (c : rest) line offset
              | c == ';' = matchComment
              | isAlpha c = matchIdentifier
              | isOperator c = matchOperator
              | isWhitespace c = matchSpaceAndDiscard
              | isNewline c = matchNewline
              | isDigit c = matchConstant
              | isDollar c = matchHexConstant
              | isPercent c = matchBinConstant
              | isQuote c = matchStringConstant
              | isDot c = matchLabel
              | otherwise = throwLexError ("Unrecognised character '" ++ [c] ++ "'") line offset

          match a b c = error (show a ++ " " ++ show b ++ " " ++ show c)

tokeniseAsmCode :: CodeTransformation ()
tokeniseAsmCode =
    do done <- gets lexDone
       if done
       then return ()
       else do characters <- gets lexStateCharacters
               lineNumber <- gets lexStateLineNumber
               lineOffset <- gets lexStateLineOffset
               tokens <- gets lexStateTokens

               let continue =
                     do match characters lineNumber lineOffset
                        resetLexState
                        tokeniseAsmCode

               if not (Seq.null tokens)
               then do let (_ Seq.:> item) = Seq.viewr tokens
                       if tokenID item == TOKEN_END_ASM
                       then do resetLexState
                       else do continue
               else continue

    where match (c : _) line offset
              | isAlpha c = matchIdentifierAsm
              | isWhitespace c = matchSpace
              | isNewline c = matchNewline
              | otherwise = matchAnything

lexDone :: CodeState -> Bool
lexDone lexState = lexStateCharacters lexState == ""

resetLexState :: CodeTransformation ()
resetLexState =
        do state <- get
           put state {lexStateID = LEX_PENDING, lexStateCurrentToken = emptyToken}

lexerPushIncludeFileName :: String -> CodeTransformation ()
lexerPushIncludeFileName fileName =
  do state <- get
     put $ state {lexStateIncludeFileNameStack =
                  (fileName:lexStateIncludeFileNameStack state)}

lexerPopIncludeFileName :: CodeTransformation ()
lexerPopIncludeFileName =
  do state <- get
     put $ state {lexStateIncludeFileNameStack =
                  tail (lexStateIncludeFileNameStack state)}

removeQuotes :: String -> String
removeQuotes string = take ((length string) - 2) (tail string)

extractCharacterFromSet :: String -> String -> (String, String)
extractCharacterFromSet characters string =
    if string /= [] && head string `elem` characters
    then ([head string], tail string)
    else ("", string)

extractCharacterNotFromSet :: String -> String -> (String, String)
extractCharacterNotFromSet characters string =
    if string /= [] && (not (head string `elem` characters))
    then ([head string], tail string)
    else ("", string)

extractAnyCharacterFromSet :: String -> String -> (String, String)
extractAnyCharacterFromSet characters string =
    if string /= [] && head string `elem` characters
    then ([head characters], tail string)
    else ("", string)

isWhitespace :: Char -> Bool
isWhitespace char = isSpace char && not (char == '\n')

isQuote :: Char -> Bool
isQuote char = char == '\"'

isOperator :: Char -> Bool
isOperator char = char `elem` "+-*/^~><=():,\\"

isDot :: Char -> Bool
isDot char = char == '.'

isDollar :: Char -> Bool
isDollar char = char == '$'

isPercent :: Char -> Bool
isPercent char = char == '%'

isBinDigit :: Char -> Bool
isBinDigit char = char `elem` "01"

isNewline :: Char -> Bool
isNewline char = char == '\n'

matchComment :: CodeTransformation ()
matchComment =

    do consume (span isPlainText)
         LEX_PENDING LEX_PENDING []
         TOKEN_COMMENT
       token <- gets lexStateCurrentToken
       state <- get
       let newToken = Token TOKEN_COMMENT (tokenValue token) (tokenLineNumber token) (tokenOffset token)
       put $ state {lexStateCurrentToken = newToken}

    where notNewline s = (extractCharacterNotFromSet "\n" s)
          isPlainText char = char /= '\n'

matchKeyword :: CodeTransformation ()
matchKeyword =

    do token <- gets lexStateCurrentToken
       let container = Map.lookup (map toLower (tokenValue token)) keywordMap
       if container == Nothing
       then return ()
       else do state <- get
               let keyword = fromJust container
                   newToken = Token (keywordTokenID keyword) (tokenValue token) (tokenLineNumber token) (tokenOffset token)
               put $ state {lexStateCurrentToken = newToken}

matchOperator :: CodeTransformation ()
matchOperator =

    do characters <- gets lexStateCharacters
       let operators = map snd operatorList
           matches = filter (\ k -> keywordValue k `isPrefixOf` characters) operators
       if null matches
       then do line <- gets lexStateLineNumber
               offset <- gets lexStateLineOffset
               throwLexError "Unrecognised operator." line offset
       else do state <- get
               lineNumber <- gets lexStateLineNumber
               offset <- gets lexStateLineOffset

               let candidates = zip (map (length . keywordValue) matches) matches
                   match = snd (maximumBy (compare `on` fst) candidates)
                   token = Token {tokenID = (keywordTokenID match),
                                  tokenValue = (keywordValue match),
                                  tokenLineNumber = lineNumber,
                                  tokenOffset = offset}
               put $ state {lexStateCurrentToken = token,
                            lexStateCharacters = drop (length (tokenValue token)) characters,
                            lexStateLineOffset = lexStateLineOffset state + (length (tokenValue token))}
               store

matchIdentifier :: CodeTransformation ()
matchIdentifier =

    do consume alphaNumUnderscore
               LEX_PENDING LEX_PENDING []
               TOKEN_IDENTIFIER

       consume typeTag
               LEX_DONE LEX_PENDING []
               TOKEN_IDENTIFIER

       consume customTypeTag
               LEX_PENDING LEX_DONE []
               TOKEN_IDENTIFIER

       consume alpha
               LEX_PENDING LEX_ERROR "Expecting Type name identifier."
               TOKEN_IDENTIFIER

       consume alphaNumUnderscore
               LEX_DONE LEX_DONE []
               TOKEN_IDENTIFIER       
      
       matchKeyword
       store

    where alphaNumUnderscore = span isAlphaNumUnderscore
          alpha = span isAlpha
          typeTag s = extractCharacterFromSet "%#$" s
          customTypeTag s = extractCharacterFromSet "." s
          isAlphaNumUnderscore char = isAlpha (char) || isDigit (char) || char == '_'

matchIdentifierAsm :: CodeTransformation ()
matchIdentifierAsm =

    do consume alphaNumUnderscore
               LEX_DONE LEX_DONE []
               TOKEN_IDENTIFIER

       matchKeyword
       store
       
    where alphaNumUnderscore = span isAlphaNumUnderscore
          isAlphaNumUnderscore char = isAlpha (char) || isDigit (char) || char == '_'

matchSpaceAndDiscard :: CodeTransformation ()
matchSpaceAndDiscard =

    do consume (span isWhitespace)
               LEX_DONE LEX_DONE []
               TOKEN_SPACE

       discard

matchSpace :: CodeTransformation ()
matchSpace =

    do consume (span isWhitespace)
               LEX_DONE LEX_DONE []
               TOKEN_SPACE

       store

matchAnything :: CodeTransformation ()
matchAnything =

    do consume (span isAnything)
               LEX_DONE LEX_DONE []
               TOKEN_ANYTHING

       store
       
       where isAnything c = not (isWhitespace c || isAlpha c || c == '\n')

matchNewline :: CodeTransformation ()
matchNewline =

    do consume newline
               LEX_DONE LEX_DONE []
               TOKEN_NEWLINE

       store

       state <- get
       put state { lexStateLineOffset = 0}

    where newline s = extractCharacterFromSet "\n" s

matchConstant :: CodeTransformation ()
matchConstant =

    do consume integral
               LEX_PENDING LEX_PENDING []
               TOKEN_INT_CONSTANT

       consume decimalPoint
               LEX_PENDING LEX_DONE []
               TOKEN_FLOAT_CONSTANT

       consume fractional
               LEX_PENDING LEX_ERROR "Expecting fractional part of floating point number."
               TOKEN_FLOAT_CONSTANT

       consume e
               LEX_PENDING LEX_DONE []
               TOKEN_FLOAT_CONSTANT

       consume negative
               LEX_PENDING LEX_PENDING []
               TOKEN_FLOAT_CONSTANT

       consume exponent_
               LEX_DONE LEX_ERROR "Expecting integer exponent."
               TOKEN_FLOAT_CONSTANT

       store

    where integral = span isDigit
          decimalPoint s = extractCharacterFromSet "." s
          fractional = span isDigit
          e s = extractAnyCharacterFromSet "eE" s
          negative s = extractCharacterFromSet "-" s
          exponent_ = span isDigit

matchHexConstant :: CodeTransformation ()
matchHexConstant =

    do consume dollar
               LEX_PENDING LEX_PENDING []
               TOKEN_HEX_CONSTANT

       consume hexDigits
               LEX_PENDING LEX_ERROR "Expecting hexadecimal constant."
               TOKEN_HEX_CONSTANT

       store

    where dollar s = extractCharacterFromSet "$" s
          hexDigits = span isHexDigit

matchBinConstant :: CodeTransformation ()
matchBinConstant =

    do consume percent
               LEX_PENDING LEX_PENDING []
               TOKEN_BIN_CONSTANT

       consume binDigits
               LEX_PENDING LEX_ERROR "Expecting binary constant."
               TOKEN_BIN_CONSTANT

       store

    where percent s = extractCharacterFromSet "%" s
          binDigits = span isBinDigit

matchStringConstant :: CodeTransformation ()
matchStringConstant =

    do consume doubleQuote
               LEX_PENDING LEX_PENDING []
               TOKEN_STRING_CONSTANT

       consume (span isPlainText)
               LEX_PENDING LEX_PENDING []
               TOKEN_STRING_CONSTANT

       consume doubleQuote
               LEX_DONE LEX_ERROR "Expecting terminating double-quote."
               TOKEN_STRING_CONSTANT

       token <- gets lexStateCurrentToken

       store

    where doubleQuote s = extractCharacterFromSet "\"" s
          isPlainText char = char /= '\"' && char /= '\n'

matchLabel :: CodeTransformation ()
matchLabel =

    do consume dot
               LEX_PENDING LEX_PENDING []
               TOKEN_LABEL

       consume (span isAlpha)
               LEX_PENDING LEX_ERROR "Expecting identifier."
               TOKEN_LABEL

       consume alphaNumUnderscore
               LEX_DONE LEX_DONE []
               TOKEN_LABEL
               
       store

    where dot s = extractCharacterFromSet "." s
          alphaNumUnderscore = span isAlphaNumUnderscore
          isAlphaNumUnderscore char = isAlpha (char) || isDigit (char) || char == '_'

store :: CodeTransformation ()
store =

    do token <- gets lexStateCurrentToken
       tokens <- gets lexStateTokens
       pendingTokens <- gets lexStatePendingTokens
       offset <- gets lexStateLineOffset
       let newToken = token {tokenOffset = (offset - length (tokenValue token))}
       if tokenID token == TOKEN_NEWLINE
       then do s <- get
               put s {lexStateLineNumber = lexStateLineNumber s + 1} 
       else return ()

       compoundTokens <- gets lexStateCompoundTokens
       let matches = filter (\ct -> (head (compoundTokenIDs ct)) == tokenID token) compoundTokens

       if matches /= []
       then do let compoundToken = head matches
               if length (compoundTokenIDs compoundToken) == 1
               then do state <- get
                       put state {lexStateID = LEX_DONE,
                                  lexStateTokens = tokens Seq.|> (mergeTokens (Foldable.toList (pendingTokens Seq.|> newToken)) (compoundTokenID compoundToken)),
                                  lexStatePendingTokens = Seq.empty,
                                  lexStateCompoundTokens = allCompoundTokens}
                                  
               else do state <- get
                       let continueCompoundTokens compoundToken = CompoundToken (compoundTokenID compoundToken) (tail (compoundTokenIDs compoundToken))
                           newCompoundTokens = map continueCompoundTokens matches
                       put state {lexStateCompoundTokens = newCompoundTokens,
                                  lexStatePendingTokens = (pendingTokens Seq.|> newToken)}
       else do if Seq.length pendingTokens > 0
               then do 
                       let (item Seq.:< rest) = Seq.viewl pendingTokens
                       state <- get
                       put state {lexStateID = LEX_DONE,
                                  lexStateTokens = (tokens Seq.|> item) Seq.|> newToken,
                                  lexStatePendingTokens = rest,
                                  lexStateCompoundTokens = allCompoundTokens}
               else do return ()
                       state <- get
                       put state {lexStateID = LEX_DONE,
                                  lexStateTokens = tokens Seq.|> newToken,
                                  lexStateCompoundTokens = allCompoundTokens}
              
mergeTokens :: [Token] -> TokenID -> Token
mergeTokens (token:tokens) id =
  Token {tokenID = id,
         tokenValue = concatMap tokenValue (token:tokens),
         tokenLineNumber = tokenLineNumber token,
         tokenOffset = tokenOffset token}

discard :: CodeTransformation ()
discard =

    do state <- get
       put state {lexStateID = LEX_DONE}

consume :: ConsumeFunction -> LexStateID -> LexStateID -> String -> TokenID -> CodeTransformation ()
consume consumeFunction successState failureState errorMessage newTokenID =

    do tokens <- gets lexStatePendingTokens
       id <- gets lexStateID
       token <- gets lexStateCurrentToken
       line <- gets lexStateLineNumber
       offset <- gets lexStateLineOffset
       characters <- gets lexStateCharacters
       state <- get
       let (k,l) = consumeFunction characters
       case id of
            LEX_DONE -> return ()
            _ -> if k /= [] 
                 then put state
                          {lexStateID = successState,
                           lexStateCurrentToken = Token {tokenID = newTokenID,
                                                         tokenValue = (tokenValue token ++ k),
                                                         tokenLineNumber = line,
                                                         tokenOffset = 0},
                           lexStateLineOffset = offset + (length $ k),
                           lexStateCharacters = l}
                           
                 else if failureState == LEX_ERROR
                      then do throwLexError errorMessage line offset -- (FatalError line offset errorMessage)
                      else put state {lexStateID = failureState}

throwLexError :: String -> Int -> Int -> CodeTransformation ()
throwLexError msg lineNumber offset =
  do state <- get
     let fileName = head (lexStateIncludeFileNameStack state)
     throwError (FatalError fileName lineNumber offset msg)
