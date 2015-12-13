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


module StandardFunctions where

import ParserData
import SemanticsData
import CompilerData

import qualified Data.Map as Map

declareStandardFunction :: String -> VariableType -> [Parameter] -> (String,Symbol)
declareStandardFunction name returnType parameterList =

        (name,Function ("bb_" ++ name) name returnType FUNCTION_ORIGIN_STANDARD parameterList (length parameterList) (length parameterList) 0 0 0 False Map.empty)

declareStandardFunctionVar :: String -> VariableType -> [Parameter] -> Int -> Int -> (String,Symbol)
declareStandardFunctionVar name returnType parameterList maxNumArguments minNumArguments =

        (name,Function ("bb_" ++ name) name returnType FUNCTION_ORIGIN_STANDARD parameterList maxNumArguments minNumArguments 0 0 0 False Map.empty)

createRequiredParameter :: String -> VariableType -> Parameter
createRequiredParameter name dataType =
  Parameter name dataType EmptyStatement

createOptionalParameter :: String -> VariableType -> Statement -> Parameter
createOptionalParameter name dataType statement =
  Parameter name dataType statement

idlewildLangStandardFunctions =
  Map.fromList
     [declareStandardFunction "init_libkoshka_mm" VARIABLE_TYPE_INT
        [createRequiredParameter [] VARIABLE_TYPE_INT],
      declareStandardFunction "final_libkoshka_mm" VARIABLE_TYPE_INT [],
      declareStandardFunction "init_libkoshka_core" VARIABLE_TYPE_INT
        [createRequiredParameter [] VARIABLE_TYPE_INT],
      declareStandardFunction "final_libkoshka_core" VARIABLE_TYPE_INT [],

      declareStandardFunction "createbank" VARIABLE_TYPE_INT
        [createRequiredParameter [] VARIABLE_TYPE_INT],
      declareStandardFunction "freebank" VARIABLE_TYPE_INT
        [createRequiredParameter [] VARIABLE_TYPE_INT],      
      declareStandardFunction "resizebank" VARIABLE_TYPE_INT
        [createRequiredParameter [] VARIABLE_TYPE_INT,
         createRequiredParameter [] VARIABLE_TYPE_INT],
      declareStandardFunction "copybank" VARIABLE_TYPE_INT
        [createRequiredParameter [] VARIABLE_TYPE_INT,
         createRequiredParameter [] VARIABLE_TYPE_INT,
         createRequiredParameter [] VARIABLE_TYPE_INT,
         createRequiredParameter [] VARIABLE_TYPE_INT,
         createRequiredParameter [] VARIABLE_TYPE_INT],
      declareStandardFunction "peekbyte" VARIABLE_TYPE_INT
        [createRequiredParameter [] VARIABLE_TYPE_INT,
         createRequiredParameter [] VARIABLE_TYPE_INT],
      declareStandardFunction "peekshort" VARIABLE_TYPE_INT
        [createRequiredParameter [] VARIABLE_TYPE_INT,
         createRequiredParameter [] VARIABLE_TYPE_INT],
      declareStandardFunction "peekint" VARIABLE_TYPE_INT
        [createRequiredParameter [] VARIABLE_TYPE_INT,
         createRequiredParameter [] VARIABLE_TYPE_INT],
      declareStandardFunction "peeklong" VARIABLE_TYPE_INT
        [createRequiredParameter [] VARIABLE_TYPE_INT,
         createRequiredParameter [] VARIABLE_TYPE_INT],
      declareStandardFunction "peekfloat" VARIABLE_TYPE_FLOAT
        [createRequiredParameter [] VARIABLE_TYPE_INT,
         createRequiredParameter [] VARIABLE_TYPE_INT],
      declareStandardFunction "peekdouble" VARIABLE_TYPE_FLOAT
        [createRequiredParameter [] VARIABLE_TYPE_INT,
         createRequiredParameter [] VARIABLE_TYPE_INT],
      declareStandardFunction "pokebyte" VARIABLE_TYPE_INT
        [createRequiredParameter [] VARIABLE_TYPE_INT,
         createRequiredParameter [] VARIABLE_TYPE_INT,
         createRequiredParameter [] VARIABLE_TYPE_INT],
      declareStandardFunction "pokeshort" VARIABLE_TYPE_INT
        [createRequiredParameter [] VARIABLE_TYPE_INT,
         createRequiredParameter [] VARIABLE_TYPE_INT,
         createRequiredParameter [] VARIABLE_TYPE_INT],
      declareStandardFunction "pokeint" VARIABLE_TYPE_INT
        [createRequiredParameter [] VARIABLE_TYPE_INT,
         createRequiredParameter [] VARIABLE_TYPE_INT,
         createRequiredParameter [] VARIABLE_TYPE_INT],
      declareStandardFunction "pokelong" VARIABLE_TYPE_INT
        [createRequiredParameter [] VARIABLE_TYPE_INT,
         createRequiredParameter [] VARIABLE_TYPE_INT,
         createRequiredParameter [] VARIABLE_TYPE_INT],
      declareStandardFunction "pokefloat" VARIABLE_TYPE_FLOAT
        [createRequiredParameter [] VARIABLE_TYPE_INT,
         createRequiredParameter [] VARIABLE_TYPE_INT,
         createRequiredParameter [] VARIABLE_TYPE_FLOAT],
      declareStandardFunction "pokedouble" VARIABLE_TYPE_FLOAT
        [createRequiredParameter [] VARIABLE_TYPE_INT,
         createRequiredParameter [] VARIABLE_TYPE_INT,
         createRequiredParameter [] VARIABLE_TYPE_FLOAT],
      
      declareStandardFunctionVar "graphics" VARIABLE_TYPE_INT
        [createRequiredParameter [] VARIABLE_TYPE_INT,
         createRequiredParameter [] VARIABLE_TYPE_INT,
         createOptionalParameter [] VARIABLE_TYPE_INT (createMinimalStatement EXPRESSION_INT_CONSTANT [IntConstantExpression (-1)]),
         createOptionalParameter [] VARIABLE_TYPE_INT (createMinimalStatement EXPRESSION_INT_CONSTANT [IntConstantExpression (-1)])] 4 2,
      declareStandardFunction "setbuffer" VARIABLE_TYPE_INT
        [createRequiredParameter [] VARIABLE_TYPE_INT],
      declareStandardFunction "backbuffer" VARIABLE_TYPE_INT
        [],
      declareStandardFunctionVar "clscolor" VARIABLE_TYPE_INT
        [createRequiredParameter [] VARIABLE_TYPE_INT,
         createRequiredParameter [] VARIABLE_TYPE_INT,
         createRequiredParameter [] VARIABLE_TYPE_INT,
         createOptionalParameter [] VARIABLE_TYPE_INT (createMinimalStatement EXPRESSION_INT_CONSTANT [IntConstantExpression (255)])] 4 3,
      declareStandardFunctionVar "color" VARIABLE_TYPE_INT
        [createRequiredParameter [] VARIABLE_TYPE_INT,
         createRequiredParameter [] VARIABLE_TYPE_INT,
         createRequiredParameter [] VARIABLE_TYPE_INT,
         createOptionalParameter [] VARIABLE_TYPE_INT (createMinimalStatement EXPRESSION_INT_CONSTANT [IntConstantExpression (255)])] 4 3,
      declareStandardFunction "cls" VARIABLE_TYPE_INT [],
      declareStandardFunctionVar "flip" VARIABLE_TYPE_INT
        [createOptionalParameter [] VARIABLE_TYPE_INT (createMinimalStatement EXPRESSION_INT_CONSTANT [IntConstantExpression (0)])] 1 0,
      declareStandardFunction "line" VARIABLE_TYPE_INT
        [createRequiredParameter [] VARIABLE_TYPE_INT,
         createRequiredParameter [] VARIABLE_TYPE_INT,
         createRequiredParameter [] VARIABLE_TYPE_INT,
         createRequiredParameter [] VARIABLE_TYPE_INT],
      declareStandardFunction "origin" VARIABLE_TYPE_INT
        [createRequiredParameter [] VARIABLE_TYPE_INT,
         createRequiredParameter [] VARIABLE_TYPE_INT],
      declareStandardFunction "setscale" VARIABLE_TYPE_INT
        [createRequiredParameter [] VARIABLE_TYPE_FLOAT,
         createRequiredParameter [] VARIABLE_TYPE_FLOAT],
      declareStandardFunction "setorientation" VARIABLE_TYPE_INT
        [createRequiredParameter [] VARIABLE_TYPE_FLOAT],
      declareStandardFunction "setalpha" VARIABLE_TYPE_INT
        [createRequiredParameter [] VARIABLE_TYPE_FLOAT],
      declareStandardFunction "getscalex" VARIABLE_TYPE_FLOAT [],      
      declareStandardFunction "getscaley" VARIABLE_TYPE_FLOAT [],
      declareStandardFunction "getorientation" VARIABLE_TYPE_FLOAT [],
      declareStandardFunction "getalpha" VARIABLE_TYPE_FLOAT [],
      
      declareStandardFunction "loadimage" VARIABLE_TYPE_INT
        [createRequiredParameter [] VARIABLE_TYPE_STRING],
      declareStandardFunction "maskimage" VARIABLE_TYPE_INT
        [createRequiredParameter [] VARIABLE_TYPE_INT,
         createRequiredParameter [] VARIABLE_TYPE_INT,
         createRequiredParameter [] VARIABLE_TYPE_INT,
         createRequiredParameter [] VARIABLE_TYPE_INT],
      declareStandardFunction "handleimage" VARIABLE_TYPE_INT
        [createRequiredParameter [] VARIABLE_TYPE_INT,
         createRequiredParameter [] VARIABLE_TYPE_INT,
         createRequiredParameter [] VARIABLE_TYPE_INT],
      declareStandardFunction "imagexhandle" VARIABLE_TYPE_INT
        [createRequiredParameter [] VARIABLE_TYPE_INT],
      declareStandardFunction "imageyhandle" VARIABLE_TYPE_INT
        [createRequiredParameter [] VARIABLE_TYPE_INT],
      declareStandardFunction "imagewidth" VARIABLE_TYPE_INT
        [createRequiredParameter [] VARIABLE_TYPE_INT],
      declareStandardFunction "imageheight" VARIABLE_TYPE_INT
        [createRequiredParameter [] VARIABLE_TYPE_INT],
      declareStandardFunction "midhandle" VARIABLE_TYPE_INT
        [createRequiredParameter [] VARIABLE_TYPE_INT],
      declareStandardFunction "automidhandle" VARIABLE_TYPE_INT
        [createRequiredParameter [] VARIABLE_TYPE_INT],
      declareStandardFunctionVar "drawimage" VARIABLE_TYPE_INT
        [createRequiredParameter [] VARIABLE_TYPE_INT,
         createRequiredParameter [] VARIABLE_TYPE_INT,
         createRequiredParameter [] VARIABLE_TYPE_INT,
         createOptionalParameter [] VARIABLE_TYPE_INT (createMinimalStatement EXPRESSION_INT_CONSTANT [IntConstantExpression 0])] 4 3,
      declareStandardFunctionVar "drawimagerect" VARIABLE_TYPE_INT
        [createRequiredParameter [] VARIABLE_TYPE_INT,
         createRequiredParameter [] VARIABLE_TYPE_INT,
         createRequiredParameter [] VARIABLE_TYPE_INT,
         createRequiredParameter [] VARIABLE_TYPE_INT,
         createRequiredParameter [] VARIABLE_TYPE_INT,
         createRequiredParameter [] VARIABLE_TYPE_INT,
         createRequiredParameter [] VARIABLE_TYPE_INT,
         createOptionalParameter [] VARIABLE_TYPE_INT (createMinimalStatement EXPRESSION_INT_CONSTANT [IntConstantExpression 0])] 8 7,
      declareStandardFunctionVar "oval" VARIABLE_TYPE_INT
        [createRequiredParameter [] VARIABLE_TYPE_INT,
         createRequiredParameter [] VARIABLE_TYPE_INT,
         createRequiredParameter [] VARIABLE_TYPE_INT,
         createRequiredParameter [] VARIABLE_TYPE_INT,
         createOptionalParameter [] VARIABLE_TYPE_INT (createMinimalStatement EXPRESSION_INT_CONSTANT [IntConstantExpression 0])] 5 4,
      declareStandardFunctionVar "rect" VARIABLE_TYPE_INT
        [createRequiredParameter [] VARIABLE_TYPE_INT,
         createRequiredParameter [] VARIABLE_TYPE_INT,
         createRequiredParameter [] VARIABLE_TYPE_INT,
         createRequiredParameter [] VARIABLE_TYPE_INT,
         createOptionalParameter [] VARIABLE_TYPE_INT (createMinimalStatement EXPRESSION_INT_CONSTANT [IntConstantExpression 0])] 5 4,
            
      declareStandardFunctionVar "loadfont" VARIABLE_TYPE_INT
        [createRequiredParameter [] VARIABLE_TYPE_STRING,
         createRequiredParameter [] VARIABLE_TYPE_INT,
         createRequiredParameter [] VARIABLE_TYPE_INT,
         createOptionalParameter [] VARIABLE_TYPE_INT (createMinimalStatement EXPRESSION_INT_CONSTANT [IntConstantExpression 0]),
         createOptionalParameter [] VARIABLE_TYPE_INT (createMinimalStatement EXPRESSION_INT_CONSTANT [IntConstantExpression 0])] 5 3,
      declareStandardFunction "setfont" VARIABLE_TYPE_INT
        [createRequiredParameter [] VARIABLE_TYPE_INT],
      declareStandardFunctionVar "text" VARIABLE_TYPE_INT
        [createRequiredParameter [] VARIABLE_TYPE_INT,
         createRequiredParameter [] VARIABLE_TYPE_INT,
         createRequiredParameter [] VARIABLE_TYPE_STRING,
         createOptionalParameter [] VARIABLE_TYPE_INT (createMinimalStatement EXPRESSION_INT_CONSTANT [IntConstantExpression 0]),
         createOptionalParameter [] VARIABLE_TYPE_INT (createMinimalStatement EXPRESSION_INT_CONSTANT [IntConstantExpression 0])] 5 3,
      declareStandardFunction "stringwidth" VARIABLE_TYPE_INT
        [createRequiredParameter [] VARIABLE_TYPE_STRING],
      declareStandardFunction "stringheight" VARIABLE_TYPE_INT
        [createRequiredParameter [] VARIABLE_TYPE_STRING],

      declareStandardFunction "loadsound" VARIABLE_TYPE_INT
        [createRequiredParameter [] VARIABLE_TYPE_STRING],
      declareStandardFunction "playsound" VARIABLE_TYPE_INT
        [createRequiredParameter [] VARIABLE_TYPE_INT],
      declareStandardFunction "loopsound" VARIABLE_TYPE_INT
        [createRequiredParameter [] VARIABLE_TYPE_INT],
      declareStandardFunction "stopchannel" VARIABLE_TYPE_INT
        [createRequiredParameter [] VARIABLE_TYPE_INT],
      declareStandardFunction "channelplaying" VARIABLE_TYPE_INT
        [createRequiredParameter [] VARIABLE_TYPE_INT],
      
      declareStandardFunction "processevents" VARIABLE_TYPE_INT
        [],   
      declareStandardFunction "keydown" VARIABLE_TYPE_INT
        [createRequiredParameter [] VARIABLE_TYPE_INT],      
      declareStandardFunction "keyhit" VARIABLE_TYPE_INT
        [createRequiredParameter [] VARIABLE_TYPE_INT],

      declareStandardFunction "setprecision" VARIABLE_TYPE_INT
        [createRequiredParameter [] VARIABLE_TYPE_INT],        
      declareStandardFunction "abs" VARIABLE_TYPE_FLOAT
        [createRequiredParameter [] VARIABLE_TYPE_FLOAT],
      declareStandardFunction "sin" VARIABLE_TYPE_FLOAT
        [createRequiredParameter [] VARIABLE_TYPE_FLOAT],
      declareStandardFunction "cos" VARIABLE_TYPE_FLOAT
        [createRequiredParameter [] VARIABLE_TYPE_FLOAT],
      declareStandardFunction "atan2" VARIABLE_TYPE_FLOAT
        [createRequiredParameter [] VARIABLE_TYPE_FLOAT,
         createRequiredParameter [] VARIABLE_TYPE_FLOAT],
      declareStandardFunction "atan" VARIABLE_TYPE_FLOAT
        [createRequiredParameter [] VARIABLE_TYPE_FLOAT],            
      declareStandardFunction "tan" VARIABLE_TYPE_FLOAT
        [createRequiredParameter [] VARIABLE_TYPE_FLOAT],            
      declareStandardFunction "pow" VARIABLE_TYPE_FLOAT
        [createRequiredParameter [] VARIABLE_TYPE_FLOAT,
         createRequiredParameter [] VARIABLE_TYPE_FLOAT],
      declareStandardFunction "fmod" VARIABLE_TYPE_FLOAT
        [createRequiredParameter [] VARIABLE_TYPE_FLOAT,
         createRequiredParameter [] VARIABLE_TYPE_FLOAT],
      declareStandardFunction "sgn" VARIABLE_TYPE_FLOAT
        [createRequiredParameter [] VARIABLE_TYPE_FLOAT],                     
      declareStandardFunction "exp" VARIABLE_TYPE_FLOAT
        [createRequiredParameter [] VARIABLE_TYPE_FLOAT],           
      declareStandardFunction "log" VARIABLE_TYPE_FLOAT
        [createRequiredParameter [] VARIABLE_TYPE_FLOAT],           
      declareStandardFunction "sqr" VARIABLE_TYPE_FLOAT
        [createRequiredParameter [] VARIABLE_TYPE_FLOAT],           
      declareStandardFunction "seedrnd" VARIABLE_TYPE_FLOAT
        [createRequiredParameter [] VARIABLE_TYPE_INT],
      declareStandardFunction "rnd" VARIABLE_TYPE_FLOAT
        [createRequiredParameter [] VARIABLE_TYPE_FLOAT,
         createRequiredParameter [] VARIABLE_TYPE_FLOAT],           
      declareStandardFunction "rand" VARIABLE_TYPE_INT
        [createRequiredParameter [] VARIABLE_TYPE_INT,
         createRequiredParameter [] VARIABLE_TYPE_INT],
      declareStandardFunction "createtimer" VARIABLE_TYPE_INT
        [createRequiredParameter [] VARIABLE_TYPE_INT],
      declareStandardFunction "waittimer" VARIABLE_TYPE_INT
        [createRequiredParameter [] VARIABLE_TYPE_INT],         
      declareStandardFunction "lset" VARIABLE_TYPE_STRING
        [createRequiredParameter [] VARIABLE_TYPE_STRING,
         createRequiredParameter [] VARIABLE_TYPE_INT],  
      declareStandardFunction "rset" VARIABLE_TYPE_STRING
        [createRequiredParameter [] VARIABLE_TYPE_STRING,
         createRequiredParameter [] VARIABLE_TYPE_INT],  
      declareStandardFunction "len" VARIABLE_TYPE_INT
        [createRequiredParameter [] VARIABLE_TYPE_STRING],             
      declareStandardFunction "mid" VARIABLE_TYPE_STRING
        [createRequiredParameter [] VARIABLE_TYPE_STRING,
         createRequiredParameter [] VARIABLE_TYPE_INT,
         createRequiredParameter [] VARIABLE_TYPE_INT],           
      declareStandardFunction "left" VARIABLE_TYPE_STRING
        [createRequiredParameter [] VARIABLE_TYPE_STRING,
         createRequiredParameter [] VARIABLE_TYPE_INT],            
      declareStandardFunction "right" VARIABLE_TYPE_STRING
        [createRequiredParameter [] VARIABLE_TYPE_STRING,
         createRequiredParameter [] VARIABLE_TYPE_INT],            
      declareStandardFunction "uni" VARIABLE_TYPE_INT
        [createRequiredParameter [] VARIABLE_TYPE_STRING],           
      declareStandardFunction "asc" VARIABLE_TYPE_INT
        [createRequiredParameter [] VARIABLE_TYPE_STRING],             
      declareStandardFunction "chr" VARIABLE_TYPE_STRING
        [createRequiredParameter [] VARIABLE_TYPE_INT],
      declareStandardFunction "upper" VARIABLE_TYPE_STRING
        [createRequiredParameter [] VARIABLE_TYPE_STRING],  
      declareStandardFunction "lower" VARIABLE_TYPE_STRING
        [createRequiredParameter [] VARIABLE_TYPE_STRING],
      --declareStandardFunction "str" VARIABLE_TYPE_STRING
      --  [createRequiredParameter [] VARIABLE_TYPE_FLOAT],           
      declareStandardFunction "print" VARIABLE_TYPE_INT
        [createRequiredParameter [] VARIABLE_TYPE_STRING],
      declareStandardFunction "write" VARIABLE_TYPE_INT
        [createRequiredParameter [] VARIABLE_TYPE_STRING],
      declareStandardFunctionVar "input" VARIABLE_TYPE_STRING
        [createOptionalParameter [] VARIABLE_TYPE_STRING (createMinimalStatement EXPRESSION_STRING_CONSTANT [StringConstantExpression ""])] 1 0,
      declareStandardFunction "read" VARIABLE_TYPE_INT
        [createRequiredParameter [] VARIABLE_TYPE_INT,
         createRequiredParameter [] VARIABLE_TYPE_INT],
      declareStandardFunction "restore" VARIABLE_TYPE_INT
        [createRequiredParameter [] VARIABLE_TYPE_INT],
      declareStandardFunction "compare_strings" VARIABLE_TYPE_STRING
        [createRequiredParameter [] VARIABLE_TYPE_STRING,
         createRequiredParameter [] VARIABLE_TYPE_STRING],
      declareStandardFunction "concatenate_strings" VARIABLE_TYPE_STRING
        [createRequiredParameter [] VARIABLE_TYPE_STRING,
         createRequiredParameter [] VARIABLE_TYPE_STRING],
      declareStandardFunction "convert_int_to_string" VARIABLE_TYPE_STRING
        [createRequiredParameter [] VARIABLE_TYPE_INT],
      declareStandardFunction "convert_float_to_string" VARIABLE_TYPE_STRING
        [createRequiredParameter [] VARIABLE_TYPE_FLOAT],
      declareStandardFunction "convert_pointer_to_string" VARIABLE_TYPE_STRING
        [createRequiredParameter [] VARIABLE_TYPE_INT],
      declareStandardFunction "convert_string_to_float" VARIABLE_TYPE_FLOAT
        [createRequiredParameter [] VARIABLE_TYPE_STRING],
      declareStandardFunction "val" VARIABLE_TYPE_FLOAT
        [createRequiredParameter [] VARIABLE_TYPE_STRING],           
      declareStandardFunction "convert_string_to_int" VARIABLE_TYPE_INT
        [createRequiredParameter [] VARIABLE_TYPE_STRING],
      declareStandardFunction "free_string" VARIABLE_TYPE_INT
        [createRequiredParameter [] VARIABLE_TYPE_STRING],
      declareStandardFunction "free_strings" VARIABLE_TYPE_INT
        [createRequiredParameter [] VARIABLE_TYPE_INT,
         createRequiredParameter [] VARIABLE_TYPE_INT],
      declareStandardFunction "duplicate_string" VARIABLE_TYPE_STRING
        [createRequiredParameter [] VARIABLE_TYPE_STRING],
      declareStandardFunction "copy_string" VARIABLE_TYPE_INT
        [createRequiredParameter [] VARIABLE_TYPE_INT,
         createRequiredParameter [] VARIABLE_TYPE_STRING],
      declareStandardFunction "allocate_array" VARIABLE_TYPE_INT
        (take (arrayMaxDimensionality + 5) (repeat (createRequiredParameter [] VARIABLE_TYPE_INT))),
      declareStandardFunction "access_array" VARIABLE_TYPE_INT
        (take (arrayMaxDimensionality + 2) (repeat (createRequiredParameter [] VARIABLE_TYPE_INT))),
      declareStandardFunction "deallocate_array" VARIABLE_TYPE_INT
        [createRequiredParameter [] VARIABLE_TYPE_INT,
         createRequiredParameter [] VARIABLE_TYPE_INT,
         createRequiredParameter [] VARIABLE_TYPE_INT],
      declareStandardFunction "new" VARIABLE_TYPE_INT
        [createRequiredParameter [] VARIABLE_TYPE_INT],
      declareStandardFunction "first" VARIABLE_TYPE_INT
        [createRequiredParameter [] VARIABLE_TYPE_INT],
      declareStandardFunction "last" VARIABLE_TYPE_INT
        [createRequiredParameter [] VARIABLE_TYPE_INT],
      declareStandardFunction "before" VARIABLE_TYPE_INT
        [createRequiredParameter [] VARIABLE_TYPE_INT],
      declareStandardFunction "after" VARIABLE_TYPE_INT
        [createRequiredParameter [] VARIABLE_TYPE_INT],
      declareStandardFunction "insert_before" VARIABLE_TYPE_INT
        [createRequiredParameter [] VARIABLE_TYPE_INT,
         createRequiredParameter [] VARIABLE_TYPE_INT,
         createRequiredParameter [] VARIABLE_TYPE_INT],
      declareStandardFunction "insert_after" VARIABLE_TYPE_INT
        [createRequiredParameter [] VARIABLE_TYPE_INT,
         createRequiredParameter [] VARIABLE_TYPE_INT,
         createRequiredParameter [] VARIABLE_TYPE_INT],
      declareStandardFunction "create_linked_list" VARIABLE_TYPE_INT
        [createRequiredParameter [] VARIABLE_TYPE_INT,
         createRequiredParameter [] VARIABLE_TYPE_INT],
      declareStandardFunction "destroy_linked_list" VARIABLE_TYPE_INT
        [createRequiredParameter [] VARIABLE_TYPE_INT],
      declareStandardFunction "init_for_each" VARIABLE_TYPE_INT
        [createRequiredParameter [] VARIABLE_TYPE_INT,
         createRequiredParameter [] VARIABLE_TYPE_INT],
      declareStandardFunction "next_for_each" VARIABLE_TYPE_INT
        [createRequiredParameter [] VARIABLE_TYPE_INT],
      declareStandardFunction "final_for_each" VARIABLE_TYPE_INT
        [createRequiredParameter [] VARIABLE_TYPE_INT],
      declareStandardFunction "delete" VARIABLE_TYPE_INT
        [createRequiredParameter [] VARIABLE_TYPE_INT,
         createRequiredParameter [] VARIABLE_TYPE_INT],
      declareStandardFunction "delete_each" VARIABLE_TYPE_INT
        [createRequiredParameter [] VARIABLE_TYPE_INT],
      declareStandardFunction "test_function" VARIABLE_TYPE_STRING
        [createRequiredParameter [] VARIABLE_TYPE_STRING,
         createRequiredParameter [] VARIABLE_TYPE_STRING,
         createRequiredParameter [] VARIABLE_TYPE_STRING,
         createRequiredParameter [] VARIABLE_TYPE_STRING,
         createRequiredParameter [] VARIABLE_TYPE_STRING,
         createRequiredParameter [] VARIABLE_TYPE_STRING,
         createRequiredParameter [] VARIABLE_TYPE_STRING],
      declareStandardFunction "test_function2" VARIABLE_TYPE_FLOAT
        [createRequiredParameter [] VARIABLE_TYPE_FLOAT,
         createRequiredParameter [] VARIABLE_TYPE_FLOAT],
      declareStandardFunction "test_function_int2" VARIABLE_TYPE_INT
        (take 2 (repeat (createRequiredParameter [] VARIABLE_TYPE_INT))),
      declareStandardFunction "test_function_int7" VARIABLE_TYPE_INT
        (take 7 (repeat (createRequiredParameter [] VARIABLE_TYPE_INT))),
      declareStandardFunction "test_function_int8" VARIABLE_TYPE_INT
        (take 8 (repeat (createRequiredParameter [] VARIABLE_TYPE_INT))),
      declareStandardFunction "test_function_float1" VARIABLE_TYPE_FLOAT
        [createRequiredParameter [] VARIABLE_TYPE_FLOAT],
      declareStandardFunction "test_function_float10" VARIABLE_TYPE_INT
        [createRequiredParameter [] VARIABLE_TYPE_FLOAT,
         createRequiredParameter [] VARIABLE_TYPE_FLOAT,
         createRequiredParameter [] VARIABLE_TYPE_FLOAT,
         createRequiredParameter [] VARIABLE_TYPE_FLOAT,
         createRequiredParameter [] VARIABLE_TYPE_FLOAT,
         createRequiredParameter [] VARIABLE_TYPE_FLOAT,
         createRequiredParameter [] VARIABLE_TYPE_FLOAT,
         createRequiredParameter [] VARIABLE_TYPE_FLOAT,
         createRequiredParameter [] VARIABLE_TYPE_FLOAT,
         createRequiredParameter [] VARIABLE_TYPE_FLOAT],
      declareStandardFunction "test_function_float9" VARIABLE_TYPE_INT
        [createRequiredParameter [] VARIABLE_TYPE_FLOAT,
         createRequiredParameter [] VARIABLE_TYPE_FLOAT,
         createRequiredParameter [] VARIABLE_TYPE_FLOAT,
         createRequiredParameter [] VARIABLE_TYPE_FLOAT,
         createRequiredParameter [] VARIABLE_TYPE_FLOAT,
         createRequiredParameter [] VARIABLE_TYPE_FLOAT,
         createRequiredParameter [] VARIABLE_TYPE_FLOAT,
         createRequiredParameter [] VARIABLE_TYPE_FLOAT,
         createRequiredParameter [] VARIABLE_TYPE_FLOAT],
      declareStandardFunction "test_function_string1" VARIABLE_TYPE_INT
        [createRequiredParameter [] VARIABLE_TYPE_STRING],
      declareStandardFunction "test_function_string2" VARIABLE_TYPE_INT
        [createRequiredParameter [] VARIABLE_TYPE_STRING,
         createRequiredParameter [] VARIABLE_TYPE_STRING],
      declareStandardFunction "init_gosub" VARIABLE_TYPE_INT
        [createRequiredParameter [] VARIABLE_TYPE_INT],
      declareStandardFunction "final_gosub" VARIABLE_TYPE_INT
        [],
      declareStandardFunction "fatal_error" VARIABLE_TYPE_INT
        [createRequiredParameter [] VARIABLE_TYPE_STRING],          
      declareStandardFunction "anticipate_fatal_error" VARIABLE_TYPE_INT
        [createRequiredParameter [] VARIABLE_TYPE_STRING,
         createRequiredParameter [] VARIABLE_TYPE_INT,
         createRequiredParameter [] VARIABLE_TYPE_INT],
      declareStandardFunction "debuglog" VARIABLE_TYPE_INT
        [createRequiredParameter [] VARIABLE_TYPE_STRING],      
      --declareStandardFunctionVar "end" VARIABLE_TYPE_INT
      --  [createOptionalParameter [] VARIABLE_TYPE_INT (createMinimalStatement EXPRESSION_INT_CONSTANT [IntConstantExpression 0])] 1 0,    
      declareStandardFunction "millisecs" VARIABLE_TYPE_INT
        []]
    


