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

{-# LANGUAGE CPP #-}

module CompilerData where

import ParserData
import SemanticsData

import qualified Data.Map as Map
import qualified Data.Sequence as Seq

data AsmBucket =
     AsmBucket
     {
       asmBucketContents :: Seq.Seq String
     } deriving (Show)

data Asm =
     Asm
     {
       asmCurrentBucket :: String,
       asmBucketMap :: Map.Map String AsmBucket
     } deriving (Show)

arrayMaxDimensionality :: Int
arrayMaxDimensionality = 16

data Register =
     Register
     {
       registerName :: String,
       registerAllocations :: Int,
       registerReservedForFunctionCall :: Bool,
       registerExcluded :: Bool
     } |
     NO_REGISTER
     deriving (Show, Eq)

data CPUContext =
     CPUContext
     {
       cpuContextCurrentRegister :: Register,
       cpuContextSuggestedRegisters :: [Register],
       cpuContextNumFloatRegisters :: Int,
       cpuContextPool :: [Register],
       cpuContextOffset :: Int,
       cpuContextAligned :: Bool,
       cpuContextDataType :: VariableType,
       cpuContextFunctionCallContexts :: [FunctionCallContext]
     }
     deriving (Show)

data FunctionCallContext =
  FunctionCallContext
  {
    functionCallContextFloatOffset :: Int,
    functionCallContextLeakyOffset :: Int
  }
  deriving (Show)
    

rax = Register "rax" 0 False False
rbx = Register "rbx" 0 False False
rbp = Register "rbp" 0 False False
r12 = Register "r12" 0 False False
r13 = Register "r13" 0 False False
r14 = Register "r14" 0 False False
r15 = Register "r15" 0 False False
r8 = Register "r8" 0 False False
r9 = Register "r9" 0 False False
rcx = Register "rcx" 0 False False
rdx = Register "rdx" 0 False False
rsi = Register "rsi" 0 False False
rdi = Register "rdi" 0 False False
r10 = Register "r10" 0 False False
r11 = Register "r11" 0 False False

#if LINUX==1
functionCallRegisters = [rdi, rsi, rdx, rcx, r8, r9]
scratchRegisters = [rcx, rdx, rsi, rdi, r8, r9, r10, r11]
preservedRegisters = [rbx, rbp, r12, r13, r14, r15]
numPreservedRegisters :: Int
numPreservedRegisters = 6
#elif WINDOWS==1
functionCallRegisters = [rcx, rdx, r8, r9]
scratchRegisters = [rcx, rdx, r8, r9, r10, r11]
preservedRegisters = [rbx, rsi, rdi, rbp, r12, r13, r14, r15]
numPreservedRegisters :: Int
numPreservedRegisters = 8
#endif

multimediaRegisterNames = ["xmm0", "xmm1", "xmm2", "xmm3", "xmm4", "xmm5", "xmm6", "xmm7"]

#if LINUX==1
numFunctionCallGPRs :: Int
numFunctionCallGPRs = 6

numFunctionCallMMRs :: Int
numFunctionCallMMRs = 8
#elif WINDOWS==1
numFunctionCallGPRs :: Int
numFunctionCallGPRs = 4

numFunctionCallMMRs :: Int
numFunctionCallMMRs = 4
#endif

numFPRs :: Int
numFPRs = 8

#if LINUX==1
allRegisters = [rbx, r12, r13, r14, r15, r9, r8, rcx, rdx, rsi, rdi, r10, r11]
#elif WINDOWS==1
allRegisters = [rbx, rsi, rdi, r12, r13, r14, r15, r9, r8, rdx, rcx, r10, r11]
#endif

sizeOfShadowSpace :: Int
#if LINUX == 1
sizeOfShadowSpace = 0
#elif WINDOWS == 1
sizeOfShadowSpace = 4
#endif

id_ = "10 id"
directives_ = "20 directives"
globals_ = "30 globals"
code_ = "40 code"
functions_ = "50 functions"
data_ = "60 data"

data TempStackSpace =
     TempStackSpace
     {
       tempStackSpaceSize :: Int,
       tempStackSpaceOffset :: Int
     }
     deriving (Show)

runtimeErrorNullPointerException = "\"Null pointer exception.\""
runtimeErrorOnGotoRangeException = "\"On... Goto index out of range.\""
runtimeErrorDivisionByZeroException = "\"Attempted division by zero.\""
