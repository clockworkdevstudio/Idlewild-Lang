# Idlewild-Lang
Free, cross-platform, 64-bit BASIC compiler with multimedia-focused API

Idlewild-Lang is a free (as in freedom), general-purpose 64-bit compiler and multimedia library for Linux and Windows, focusing on 2D game programming with 3D hardware acceleration. The core language and API are based on Blitz BASIC (from Blitz Research Ltd). The compiler is complete apart from bug fixes, but the API is limited to those commands needed to build and run simple games like the demo game, Skyghost, an arcade style asteroid shooter (see below for more details about what functions exactly are implemented).

DIFFERENCES FROM BLITZ BASIC

- Idlewild-Lang supports 64-bit x86 hosts and targets.
- Idlewild-Lang is cross-platform.
- Idlewild-Lang supports Unicode in all its string functions, including the rendering of Unicode characters with .ttf fonts.
- Idlewild-Lang supports inline assembly code.
- Idlewild-Lang's API "libkoshka" is experimental and incomplete.

GENERAL LANGUAGE FEATURES

- Four native 64-bit data types (int, float `[technically, "double"]`, pointer, and UTF-8 string).
- All the operators you would expect from a modern language (Arithmetic operators: +, -, \*, /, Mod, ^ `["to-the-power-of"]`; Bitwise operators: And, Or, Xor, ~ `[bitwise complement]`; Logical operator: Not; Comparision operators: >, <, =, >=, <=, <>; Assignment operator: =; Type conversion operators: Int(), Float(), Str(); Unary negate/abs: -, +; Bit-shifting operators: Shl, Shr, Sar; Precedence override operator: (); Function call operator: f(); New operator; Type-field-access operator: \; Linked list manipulation operators: First, Last, Before, After).
- Built-in constants True, False, Pi and Null.
- Support for hexadecimal and binary constants.
- Arrays of any data type and any practical dimensionality, and the ability to re-dimension arrays.
- The freedom to define custom types with the "Type" keyword (including support for arrays of types, and types within types).
- Implicit, "behind-the-scenes" linked list support with "For... Each" loops to iterate over objects of a particular type.
- Smart "New/Delete" operators for use with types.
- Operators for sorting or otherwise rearranging linked lists.
- Full support for subroutines, including C-style functions with optional arguments.
- All the traditional looping constructs you would expect ("For... To... Step... Next","Repeat... Until/Forever","While... Wend").
- Conditional branching with "If... Then... Else If... Else" and "Select... Case... Default".
- Branching with "Goto", "Gosub" and "On... Goto/Gosub" ("On... Goto/Gosub" statement uses jump tables for constant-time O(1) branching).
- Support for global, local and constant "variables".

INSTALL FROM BINARY PACKAGES

I hope to upload binary packages soon to make installation much easier on all supported OSs.

INSTALL FROM SOURCE (LINUX)

To compile and install the language on a (RECENT!) Ubuntu host:

sudo apt-get install ghc libghc-mtl-dev libglib2.0-dev libgtk-3-dev libsdl2-dev libsdl2-image-2.0-0 libsdl2-image-dev libsdl2-ttf-2.0-0 libsdl2-ttf-dev libsdl2-mixer-2.0-0 libsdl2-mixer-dev freeglut3-dev pkg-config libglew-dev libglm-dev

make

chmod +x ./install-linux.sh

sudo ./install-linux.sh

Assuming you have a working assembler (NASM or FASM), reachable from within your PATH environment variable, this is all you need to do.

INSTALL FROM SOURCE (WINDOWS)

The same sources used to compile Idlewild-Lang on Linux can be used to build it on Windows. However, you will need to download all the disparate dependencies "manually" from the web. If you're brave enough to try a Windows build, you'll need the following .dlls

freeglut.dll

glew32.dll

libFLAC-8.dll

libfreetype-6.dll

libglib-2.0-0.dll

libiconv-2.dll

libintl-8.dll

libjpeg-9.dll

libmikmod-2.dll

libmodplug-1.dll

libogg-0.dll

libpng16-16.dll

libtiff-5.dll

libvorbis-0.dll

libvorbisfile-3.dll

libwebp-4.dll

SDL2.dll

SDL2_image.dll

SDL2_mixer.dll

SDL2_ttf.dll

smpeg2.dll

zlib1.dll

These should go in C:\Windows\System32.

In addition, you'll need all the corresponding C header files, the Glasgow Haskell Compiler, and Cygwin. You'll also need an assembler (NASM or FASM) reachable from within your PATH environment variable, as well as the linker GoLink.exe. Then you need to issue the command:

make windows

install-windows.bat

(from a Cygwin admin shell.)

USING THE COMPILER

(Assuming a Linux host) Issue the command:

idlewild-lang source.bb

A few more options are available; type:

idlewild-lang --help

for details.

In particular, the default assembler used by Idlewild-Lang is NASM because it is free. To use FASM instead, issue:

idlewild-lang source.bb --backend=fasm

You can make this choice implicit and persistent by editing the file ~/.idlewild-lang.conf

DISTRIBUTING IDLEWILD-LANG PROGRAMS

If you compiled a program with the --console option, it will have the minimal (core) dependencies (Windows assumed):

libkoshka.core.dll

libglib-2.0-0.dll

libintl-8.dll

libiconv-2.dll

Otherwise, it will have the above core dependencies and, additionally, the other .dlls mentioned above.

LIST OF BLITZ BASIC KEYWORDS AND OPERATORS IMPLEMENTED

\*Keywords marked with an asterisk are new and were not part of the original Blitz BASIC.
(Essentially all keywords/operators are implemented except the debugging command "Stop", and the undocumented commands/operators "Object", "Handle" and "`[]`")

After, And, Before, Case, Const, Data, Default, Delete, Dim, Each, Else, Else If, End, End If, Exit, False, Field, First, Float, For, Forever, Function, Global, Gosub, Goto, If,, Insert, Int, Last, Local, Mod, New, Next, Not, Null, Or, Pi, Read, Repeat, Restore, Return, Sar, Select, Shl, Shr, Step, Str, Then, To, True, Type, Until, Wend, While, Xor, Include, On\*, + (unary) , - (unary), ~, ^, \*, /, +, -, >, < , >=, <=, <>, \

LIST OF BLITZ BASIC FUNCTIONS IMPLEMENTED (OR AT LEAST PARTIALLY IMPLEMENTED)

\*Functions marked with an asterisk are new and were not part of the original Blitz BASIC; they generally deal with graphics hardware acceleration and Unicode.

Graphics, SetBuffer, BackBuffer, Cls, ClsColor, Color, Flip, Origin, Line, SetScale\*, SetOrientation\*, GetScaleX\*, GetScaleY\*, LoadImage, MaskImage, HandleImage, ImageXHandle, ImageYHandle, ImageWidth, ImageHeight, MidHandle, AutoMidHandle, DrawImage, DrawImageRect, Oval, Rect, LoadFont, SetFont, Text, StringWidth, StringHeight, LoadSound, PlaySound, LoopSound, StopChannel, ChannelPlaying, KeyDown, KeyHit, SetPrecision\*, Abs, Sin, Cos, ATan2, ATan, Tan, Sgn, Exp, Log, Sqr, SeedRnd, Rnd, Rand, CreateTimer, WaitTimer, LSet, RSet, Len, Mid, Left, Right, Uni\*, Asc, Chr, Upper, Lower, Print, Write, Input, DebugLog, Millisecs
