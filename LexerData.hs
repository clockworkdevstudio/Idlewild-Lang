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

module LexerData where

import qualified Data.Map as Map

data TokenID = 

  TOKEN_INCLUDE |
  TOKEN_INCLUDE_FILE_NAME |
  TOKEN_BEGINNING_OF_FILE |
  TOKEN_END_OF_FILE |
  TOKEN_IDENTIFIER |
  TOKEN_LABEL |
  TOKEN_INT_CONSTANT |
  TOKEN_FLOAT_CONSTANT |
  TOKEN_STRING_CONSTANT |
  TOKEN_HEX_CONSTANT |
  TOKEN_BIN_CONSTANT |
  TOKEN_PI |
  TOKEN_TRUE |
  TOKEN_FALSE |
  TOKEN_NULL |
  TOKEN_SPACE |
  TOKEN_NEWLINE |
  TOKEN_COLON |
  TOKEN_SEMICOLON |
  TOKEN_COMMENT |
  TOKEN_ASM |
  TOKEN_ASM_DATA |
  TOKEN_END_ASM |

  TOKEN_SYS |
  TOKEN_DIM |
  TOKEN_DATA |
  TOKEN_READ |
  TOKEN_RESTORE |
  TOKEN_IF |
  TOKEN_THEN |
  TOKEN_ELSE_IF |
  TOKEN_ELSE |
  TOKEN_END_IF |
  TOKEN_SELECT |
  TOKEN_CASE |
  TOKEN_DEFAULT |
  TOKEN_END_SELECT |
  TOKEN_FOR |
  TOKEN_EACH |
  TOKEN_TO |
  TOKEN_STEP |
  TOKEN_NEXT |
  TOKEN_WHILE |
  TOKEN_WEND |
  TOKEN_REPEAT |
  TOKEN_UNTIL |
  TOKEN_FOREVER |
  TOKEN_EXIT |
  TOKEN_ON |
  TOKEN_GOTO |
  TOKEN_GOSUB |
  TOKEN_RETURN |
  TOKEN_CONST |
  TOKEN_GLOBAL |
  TOKEN_LOCAL |
  TOKEN_FUNCTION |
  TOKEN_END_FUNCTION |
  TOKEN_TYPE |
  TOKEN_FIELD |
  TOKEN_END_TYPE |
  TOKEN_END |

  TOKEN_AND |
  TOKEN_OR |
  TOKEN_XOR |
  TOKEN_NOT |
  TOKEN_BITWISE_COMPLEMENT |
  TOKEN_EQUAL_TO |
  TOKEN_NOT_EQUAL_TO |
  TOKEN_GREATER_THAN |
  TOKEN_LESS_THAN |
  TOKEN_GREATER_THAN_OR_EQUAL_TO |
  TOKEN_LESS_THAN_OR_EQUAL_TO |
  TOKEN_INT |
  TOKEN_FLOAT |
  TOKEN_STR |
  TOKEN_ADD |
  TOKEN_SUB |
  TOKEN_MUL |
  TOKEN_DIV |
  TOKEN_MOD |
  TOKEN_POW |
  TOKEN_SHL |
  TOKEN_SHR |
  TOKEN_SAR |
  TOKEN_LEFT_PARENTHESIS |
  TOKEN_RIGHT_PARENTHESIS |
  TOKEN_NEW |
  TOKEN_DELETE |
  TOKEN_DELETE_EACH |
  TOKEN_INSERT |
  TOKEN_BEFORE |
  TOKEN_AFTER |
  TOKEN_FIRST |
  TOKEN_LAST |
  TOKEN_FIELD_ACCESS |
  TOKEN_COMMA |

  TOKEN_ERROR |
  TOKEN_ANYTHING |
  TOKEN_NONE |
  TOKEN_FISH
  
  deriving (Show, Eq)

data Token =
  Token {
    tokenID :: TokenID,
    tokenValue :: String,
    tokenLineNumber :: Int,
    tokenOffset :: Int
    } deriving (Show, Eq)

data CompoundToken = CompoundToken
  {compoundTokenID :: TokenID,
   compoundTokenIDs :: [TokenID]}
  deriving (Show,Eq)

allCompoundTokens =
  [CompoundToken TOKEN_DELETE_EACH [TOKEN_DELETE,TOKEN_EACH],
   CompoundToken TOKEN_INCLUDE_FILE_NAME [TOKEN_INCLUDE,TOKEN_STRING_CONSTANT],
   CompoundToken TOKEN_ASM_DATA [TOKEN_ASM,TOKEN_DATA],
   --CompoundToken TOKEN_ASM [TOKEN_ASM],
   CompoundToken TOKEN_END_ASM [TOKEN_END,TOKEN_SPACE,TOKEN_ASM],
   CompoundToken TOKEN_ELSE_IF [TOKEN_ELSE,TOKEN_IF],
   CompoundToken TOKEN_END_IF [TOKEN_END,TOKEN_IF],
   CompoundToken TOKEN_END_FUNCTION [TOKEN_END,TOKEN_FUNCTION],
   CompoundToken TOKEN_END_SELECT [TOKEN_END,TOKEN_SELECT],
   CompoundToken TOKEN_END_TYPE [TOKEN_END,TOKEN_TYPE]]

emptyToken = Token TOKEN_NONE "" 0 0

createBOFToken name =
  Token {tokenID = TOKEN_BEGINNING_OF_FILE,
         tokenValue = name,
         tokenLineNumber = 0,
         tokenOffset = 0}

createEOFToken name =
  Token {tokenID = TOKEN_END_OF_FILE,
         tokenValue = name,
         tokenLineNumber = 0,
         tokenOffset = 0}

data LexStateID = 
  LEX_PENDING |
  LEX_DONE |
  LEX_ERROR

  deriving (Show, Eq)

data Keyword =
  Keyword {
    keywordTokenID :: TokenID,
    keywordValue :: String,
    keywordLength :: Int
    } deriving (Show, Eq)

keyword :: TokenID -> String -> (String, Keyword)
keyword tokenID value = (value, Keyword tokenID value (length value))

operatorList =
  [keyword TOKEN_COLON ":",
   keyword TOKEN_BITWISE_COMPLEMENT "~",
   keyword TOKEN_COMMA ",",
   keyword TOKEN_EQUAL_TO "=",
   keyword TOKEN_NOT_EQUAL_TO "<>",
   keyword TOKEN_NOT_EQUAL_TO "><",
   keyword TOKEN_GREATER_THAN_OR_EQUAL_TO ">=",
   keyword TOKEN_GREATER_THAN_OR_EQUAL_TO "=>",
   keyword TOKEN_LESS_THAN_OR_EQUAL_TO "<=",
   keyword TOKEN_LESS_THAN_OR_EQUAL_TO "=<",
   keyword TOKEN_GREATER_THAN ">",
   keyword TOKEN_LESS_THAN "<",
   keyword TOKEN_ADD "+",
   keyword TOKEN_SUB "-",
   keyword TOKEN_MUL "*",
   keyword TOKEN_DIV "/",
   keyword TOKEN_POW "^",
   keyword TOKEN_FIELD_ACCESS "\\",
   keyword TOKEN_LEFT_PARENTHESIS "(",
   keyword TOKEN_RIGHT_PARENTHESIS ")",
   keyword TOKEN_COMMA ","]

keywordMap =
  Map.fromList
    [keyword TOKEN_INCLUDE "include",
     keyword TOKEN_SYS "sys",
     keyword TOKEN_DIM "dim",
     keyword TOKEN_DATA "data",
     keyword TOKEN_READ "read",
     keyword TOKEN_RESTORE "restore",
     keyword TOKEN_IF "if",
     keyword TOKEN_THEN "then",
     keyword TOKEN_ELSE_IF "elseif",
     keyword TOKEN_ELSE "else",
     keyword TOKEN_END_IF "endif",
     keyword TOKEN_SELECT "select",
     keyword TOKEN_CASE "case",
     keyword TOKEN_DEFAULT "default",
     keyword TOKEN_END_SELECT "endselect",
     keyword TOKEN_FOR "for",
     keyword TOKEN_EACH "each",
     keyword TOKEN_TO "to",
     keyword TOKEN_STEP "step",
     keyword TOKEN_NEXT "next",
     keyword TOKEN_WHILE "while",
     keyword TOKEN_WEND "wend",
     keyword TOKEN_REPEAT "repeat",
     keyword TOKEN_UNTIL "until",
     keyword TOKEN_FOREVER "forever",
     keyword TOKEN_EXIT "exit",
     keyword TOKEN_END "end",
     keyword TOKEN_ON "on",
     keyword TOKEN_GOTO "goto",
     keyword TOKEN_GOSUB "gosub",
     keyword TOKEN_RETURN "return",
     keyword TOKEN_CONST "const",
     keyword TOKEN_GLOBAL "global",
     keyword TOKEN_LOCAL "local",
     keyword TOKEN_FUNCTION "function",
     keyword TOKEN_END_FUNCTION "endfunction",
     keyword TOKEN_TYPE "type",
     keyword TOKEN_FIELD "field",
     keyword TOKEN_END_TYPE "endtype",
     keyword TOKEN_NEW "new",
     keyword TOKEN_DELETE "delete",
     keyword TOKEN_INSERT "insert",
     keyword TOKEN_BEFORE "before",
     keyword TOKEN_AFTER "after",
     keyword TOKEN_FIRST "first",
     keyword TOKEN_LAST "last",
     keyword TOKEN_END "end",

     keyword TOKEN_ASM "asm",
     keyword TOKEN_ASM_DATA "asmdata",
     keyword TOKEN_END_ASM "endasm",

     keyword TOKEN_PI "pi",
     keyword TOKEN_TRUE "true",
     keyword TOKEN_FALSE "false",
     keyword TOKEN_NULL "null",
     keyword TOKEN_AND "and",
     keyword TOKEN_OR "or",
     keyword TOKEN_XOR "xor",
     keyword TOKEN_NOT "not",
     keyword TOKEN_SHL "shl",
     keyword TOKEN_SHR "shr",
     keyword TOKEN_SAR "sar",
     keyword TOKEN_MOD "mod",
     keyword TOKEN_INT "int",
     keyword TOKEN_FLOAT "float",
     keyword TOKEN_STR "str",
     keyword TOKEN_FISH "fish"]

type ConsumeFunction = (String -> (String, String))
