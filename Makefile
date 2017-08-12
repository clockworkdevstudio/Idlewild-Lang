GHC_LINUX_DEBUG_FLAGS=
GHC_LINUX_RELEASE_FLAGS=-O3
GCC_LINUX_DEBUG_FLAGS=-g
GCC_LINUX_RELEASE_FLAGS=-O3
GHC_MAC_OS_DEBUG_FLAGS=
GHC_MAC_OS_RELEASE_FLAGS=-O3
GCC_MAC_OS_DEBUG_FLAGS=-g
GCC_MAC_OS_RELEASE_FLAGS=-O3
GHC_WINDOWS_DEBUG_FLAGS=
GHC_WINDOWS_RELEASE_FLAGS=-O3
GCC_WINDOWS_DEBUG_FLAGS=-g
GCC_WINDOWS_RELEASE_FLAGS=-O3
GHC_LINUX_FLAGS=
GCC_LINUX_FLAGS=
GHC_MAC_OS_FLAGS=
GCC_MAC_OS_FLAGS=
GHC_WINDOWS_FLAGS=
GCC_WINDOWS_FLAGS=

all: linux-release

.PHONY: linux-debug
linux-debug: GHC_LINUX_FLAGS+=${GHC_LINUX_DEBUG_FLAGS}
linux-debug: GCC_LINUX_FLAGS+=${GCC_LINUX_DEBUG_FLAGS}
linux-debug: build-linux

.PHONY: linux-release
linux-release: GHC_LINUX_FLAGS+=${GHC_LINUX_RELEASE_FLAGS}
linux-release: GCC_LINUX_FLAGS+=${GCC_LINUX_RELEASE_FLAGS}
linux-release: build-linux

.PHONY: mac-os-debug
mac-os-debug: GHC_MAC_OS_FLAGS+=${GHC_MAC_OS_DEBUG_FLAGS}
mac-os-debug: GCC_MAC_OS_FLAGS+=${GCC_MAC_OS_DEBUG_FLAGS}
mac-os-debug: build-mac-os

.PHONY: mac-os-release
mac-os-release: GHC_MAC_OS_FLAGS+=${GHC_MAC_OS_RELEASE_FLAGS}
mac-os-release: GCC_MAC_OS_FLAGS+=${GCC_MAC_OS_RELEASE_FLAGS}
mac-os-release: build-mac-os

.PHONY: windows-debug
windows-debug: GHC_WINDOWS_FLAGS+=${GHC_WINDOWS_DEBUG_FLAGS}
windows-debug: GCC_WINDOWS_FLAGS+=${GCC_WINDOWS_DEBUG_FLAGS}
windows-debug: build-windows

.PHONY: windows-release
windows-release: GHC_WINDOWS_FLAGS+=${GHC_WINDOWS_RELEASE_FLAGS}
windows-release: GCC_WINDOWS_FLAGS+=${GCC_WINDOWS_RELEASE_FLAGS}
windows-release: build-windows

linux: linux-release

build-linux: idlewild-lang libkoshka.core.so libkoshka.mm.so

# for profiling add: -prof -fprof-auto 
idlewild-lang: Main.hs Arguments.hs Assembler.hs Lexer.hs Parser.hs Semantics.hs Compiler.hs Linker.hs LexerData.hs ParserData.hs SemanticsData.hs CompilerData.hs StandardFunctions.hs Options.hs Common.hs
	ghc Main.hs -o idlewild-lang -DLINUX=1 ${GHC_LINUX_FLAGS}

libkoshka.core.so: libkoshka_core.o
	gcc -shared -o libkoshka.core.so libkoshka_core.o `pkg-config --libs gtk+-3.0` -lm ${GCC_LINUX_FLAGS}

libkoshka_core.o: libkoshka_core.c
	gcc -c libkoshka_core.c -DLINUX=1 -o libkoshka_core.o -fpic `pkg-config --cflags gtk+-3.0` ${GCC_LINUX_FLAGS}

libkoshka.mm.so: libkoshka_mm.o libkoshka_mm_graphics.o libkoshka_mm_sound.o libkoshka_mm_io.o libkoshka_mm_text.o libkoshka.core.so
	gcc -shared -o libkoshka.mm.so libkoshka_mm.o libkoshka_mm_graphics.o libkoshka_mm_sound.o libkoshka_mm_io.o libkoshka_mm_text.o -L. -lkoshka.core -lSDL2 -lGL -lGLU -lglut -lGLEW -lSDL2_image -lSDL2_ttf -lSDL2_mixer `pkg-config --libs gtk+-3.0` -lm ${GCC_LINUX_FLAGS}

libkoshka_mm.o: libkoshka_mm.c libkoshka_mm.h
	gcc -c libkoshka_mm.c -DLINUX=1 -o libkoshka_mm.o -fpic `pkg-config --cflags gtk+-3.0` ${GCC_LINUX_FLAGS}

libkoshka_mm_graphics.o: libkoshka_mm_graphics.cpp libkoshka_mm.h
	g++ -c libkoshka_mm_graphics.cpp -DLINUX=1 -o libkoshka_mm_graphics.o -fpic  `pkg-config --cflags gtk+-3.0` ${GCC_LINUX_FLAGS}

libkoshka_mm_sound.o: libkoshka_mm_sound.c
	gcc -c libkoshka_mm_sound.c -DLINUX=1 -o libkoshka_mm_sound.o -fpic ${GCC_LINUX_FLAGS}

libkoshka_mm_io.o: libkoshka_mm_io.c
	gcc -c libkoshka_mm_io.c -DLINUX=1 -o libkoshka_mm_io.o -fpic ${GCC_LINUX_FLAGS}

libkoshka_mm_text.o: libkoshka_mm_text.c libkoshka_mm.h
	gcc -c libkoshka_mm_text.c -DLINUX=1 -o libkoshka_mm_text.o -fpic ${GCC_LINUX_FLAGS}

mac-os: mac-os-release

build-mac-os: idlewild-lang-mac-os libkoshka.core.dylib libkoshka.mm.dylib

# for profiling add: -prof -fprof-auto 
idlewild-lang-mac-os: Main.hs Arguments.hs Assembler.hs Lexer.hs Parser.hs Semantics.hs Compiler.hs Linker.hs LexerData.hs ParserData.hs SemanticsData.hs CompilerData.hs StandardFunctions.hs Options.hs Common.hs
	ghc Main.hs -o idlewild-lang-mac-os -DMAC_OS=1 ${GHC_MAC_OS_FLAGS}

libkoshka.core.dylib: libkoshka_core_mac_os.o it_quacks_like_glib.o
	gcc -shared -o libkoshka.core.dylib libkoshka_core_mac_os.o it_quacks_like_glib.o -framework Foundation -lm ${GCC_MAC_OS_FLAGS}

libkoshka_core_mac_os.o: libkoshka_core.c
	gcc -ObjC -c libkoshka_core.c -DMAC_OS=1 -o libkoshka_core_mac_os.o -fpic ${GCC_MAC_OS_FLAGS}

libkoshka.mm.dylib: libkoshka_mm_mac_os.o libkoshka_core_mac_os.o libkoshka_mm_mac_os.o libkoshka_mm_graphics_mac_os.o libkoshka_mm_sound_mac_os.o libkoshka_mm_io_mac_os.o libkoshka_mm_text_mac_os.o it_quacks_like_glib.o libkoshka.core.dylib libkoshka.core.dylib
	gcc -shared -o libkoshka.mm.dylib libkoshka_mm_mac_os.o libkoshka_mm_graphics_mac_os.o libkoshka_mm_sound_mac_os.o libkoshka_mm_io_mac_os.o libkoshka_mm_text_mac_os.o it_quacks_like_glib.o -L. -L ../idlewild-lang-deps -lkoshka.core -lstdc++ -framework OpenGL -framework Cocoa -framework SDL2 -framework SDL2_image -framework SDL2_mixer -framework SDL2_ttf -F/Library/Frameworks -lm ${GCC_MAC_OS_FLAGS}

#-lGL -lGLU -lglut -lGLEW

libkoshka_mm_mac_os.o: libkoshka_mm.c libkoshka_mm.h
	gcc -c libkoshka_mm.c -DMAC_OS=1 -o libkoshka_mm_mac_os.o -I/usr/local/include -fpic ${GCC_MAC_OS_FLAGS}

libkoshka_mm_graphics_mac_os.o: libkoshka_mm_graphics.cpp libkoshka_mm.h
	g++ -ObjC++ -I/usr/local/include/ -I. -c libkoshka_mm_graphics.cpp -DMAC_OS=1 -D__gl_h_ -DGL_DO_NOT_WARN_IF_MULTI_GL_VERSION_HEADERS_INCLUDED -o libkoshka_mm_graphics_mac_os.o -fpic ${GCC_MAC_OS_FLAGS}

libkoshka_mm_sound_mac_os.o: libkoshka_mm_sound.c
	gcc -c libkoshka_mm_sound.c -DMAC_OS=1 -o libkoshka_mm_sound_mac_os.o -I/usr/local/include -fpic ${GCC_MAC_OS_FLAGS}

libkoshka_mm_io_mac_os.o: libkoshka_mm_io.c libkoshka_mm_io.h
	gcc -c libkoshka_mm_io.c -DMAC_OS=1 -o libkoshka_mm_io_mac_os.o -I/usr/local/include -fpic ${GCC_MAC_OS_FLAGS}

libkoshka_mm_text_mac_os.o: libkoshka_mm_text.c libkoshka_mm.h
	gcc -c libkoshka_mm_text.c -DMAC_OS=1 -o libkoshka_mm_text_mac_os.o -I/usr/local/include -fpic ${GCC_MAC_OS_FLAGS}

it_quacks_like_glib.o: it_quacks_like_glib.c it_quacks_like_glib.h
	gcc -c it_quacks_like_glib.c -DMAC_OS=1 -o it_quacks_like_glib.o -fpic ${GCC_MAC_OS_FLAGS}

#libkoshka_mm_sound.o: libkoshka_mm_sound.c
#	gcc -c libkoshka_mm_sound.c -DLINUX=1 -o libkoshka_mm_sound.o -fpic ${GCC_LINUX_FLAGS}

#libkoshka_mm_io.o: libkoshka_mm_io.c
#	gcc -c libkoshka_mm_io.c -DLINUX=1 -o libkoshka_mm_io.o -fpic ${GCC_LINUX_FLAGS}

#libkoshka_mm_text.o: libkoshka_mm_text.c libkoshka_mm.h
#	gcc -c libkoshka_mm_text.c -DLINUX=1 -o libkoshka_mm_text.o -fpic ${GCC_LINUX_FLAGS}

windows: windows-release

build-windows: idlewild-lang.exe libkoshka.core.dll libkoshka.mm.dll

# for profiling add: -prof -fprof-auto 
idlewild-lang.exe: Main.hs Arguments.hs Assembler.hs Lexer.hs Parser.hs Semantics.hs Compiler.hs Linker.hs LexerData.hs ParserData.hs SemanticsData.hs CompilerData.hs StandardFunctions.hs Options.hs Common.hs
	ghc Main.hs -o idlewild-lang -DWINDOWS=1 ${GHC_WINDOWS_FLAGS}

libkoshka.core.dll: libkoshka_core.obj
	x86_64-w64-mingw32-gcc -shared -o libkoshka.core.dll libkoshka_core.obj `pkg-config --libs glib-2.0` ${GCC_WINDOWS_FLAGS}

libkoshka_core.obj: libkoshka_core.c
	x86_64-w64-mingw32-gcc -IC:/msys64/mingw64/include `pkg-config --cflags glib-2.0` -c libkoshka_core.c -DWINDOWS=1 -DUNICODE -D_UNICODE -o libkoshka_core.obj -fpic ${GCC_WINDOWS_FLAGS}

libkoshka.mm.dll: libkoshka_mm.obj libkoshka_mm_graphics.obj libkoshka_mm_sound.obj libkoshka_mm_io.obj libkoshka_mm_text.obj libkoshka.core.dll
	x86_64-w64-mingw32-g++ -shared -o libkoshka.mm.dll libkoshka_mm.obj libkoshka_mm_graphics.obj libkoshka_mm_sound.obj libkoshka_mm_io.obj libkoshka_mm_text.obj -L. -lkoshka.core -lSDL2 -lSDL2_image -lSDL2_ttf -lSDL2_mixer -L C:/Windows/System32 `pkg-config --libs glib-2.0` -lOpenGL32 -lfreeglut -lglew32 -lmsvcrt ${GCC_WINDOWS_FLAGS}

libkoshka_mm.obj: libkoshka_mm.c
	x86_64-w64-mingw32-gcc `pkg-config --cflags glib-2.0` -c libkoshka_mm.c -DWINDOWS=1 -o libkoshka_mm.obj -fpic ${GCC_WINDOWS_FLAGS}

libkoshka_mm_graphics.obj: libkoshka_mm_graphics.cpp libkoshka_mm.h
	x86_64-w64-mingw32-g++ `pkg-config --cflags glib-2.0` -c libkoshka_mm_graphics.cpp -DWINDOWS=1 -o libkoshka_mm_graphics.obj -fpic ${GCC_WINDOWS_FLAGS}

libkoshka_mm_sound.obj: libkoshka_mm_sound.c
	x86_64-w64-mingw32-gcc -IC:/msys64/mingw64/include -c libkoshka_mm_sound.c -DWINDOWS=1 -o libkoshka_mm_sound.obj -fpic ${GCC_WINDOWS_FLAGS}

libkoshka_mm_io.obj: libkoshka_mm_io.c
	x86_64-w64-mingw32-gcc -IC:/msys64/mingw64/include -c libkoshka_mm_io.c -DWINDOWS=1 -o libkoshka_mm_io.obj -fpic ${GCC_WINDOWS_FLAGS}

libkoshka_mm_text.obj: libkoshka_mm_text.c libkoshka_mm.h
	x86_64-w64-mingw32-gcc -IC:/msys64/mingw64/include -c libkoshka_mm_text.c -DWINDOWS=1 -o libkoshka_mm_text.obj -fpic ${GCC_WINDOWS_FLAGS}

clean:
	-rm -rf *.o *.obj *.dylib *.so *.dll *.hi idlewild-lang idlewild-lang.exe idlewild-lang-mac-os nasm fasm FASM.EXE nasm.exe
