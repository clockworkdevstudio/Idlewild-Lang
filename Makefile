GHC_LINUX_DEBUG_FLAGS=
GHC_LINUX_RELEASE_FLAGS=-O3
GCC_LINUX_DEBUG_FLAGS=-g
GCC_LINUX_RELEASE_FLAGS=-O3
GHC_WINDOWS_DEBUG_FLAGS=
GHC_WINDOWS_RELEASE_FLAGS=-O3
GCC_WINDOWS_DEBUG_FLAGS=-g
GCC_WINDOWS_RELEASE_FLAGS=-O3
GHC_LINUX_FLAGS=
GCC_LINUX_FLAGS=
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

libkoshka_mm.o: libkoshka_mm.c
	gcc -c libkoshka_mm.c -DLINUX=1 -o libkoshka_mm.o -fpic `pkg-config --cflags gtk+-3.0` ${GCC_LINUX_FLAGS}

libkoshka_mm_graphics.o: libkoshka_mm_graphics.cpp libkoshka_mm.h
	g++ -c libkoshka_mm_graphics.cpp -DLINUX=1 -o libkoshka_mm_graphics.o -fpic  `pkg-config --cflags gtk+-3.0` ${GCC_LINUX_FLAGS}

libkoshka_mm_sound.o: libkoshka_mm_sound.c
	gcc -c libkoshka_mm_sound.c -DLINUX=1 -o libkoshka_mm_sound.o -fpic ${GCC_LINUX_FLAGS}

libkoshka_mm_io.o: libkoshka_mm_io.c
	gcc -c libkoshka_mm_io.c -DLINUX=1 -o libkoshka_mm_io.o -fpic ${GCC_LINUX_FLAGS}

libkoshka_mm_text.o: libkoshka_mm_text.c libkoshka_mm.h
	gcc -c libkoshka_mm_text.c -DLINUX=1 -o libkoshka_mm_text.o -fpic ${GCC_LINUX_FLAGS}

windows: windows-release

build-windows: idlewild-lang.exe libkoshka.core.dll libkoshka.mm.dll

# for profiling add: -prof -fprof-auto 
idlewild-lang.exe: Main.hs Arguments.hs Assembler.hs Lexer.hs Parser.hs Semantics.hs Compiler.hs Linker.hs LexerData.hs ParserData.hs SemanticsData.hs CompilerData.hs StandardFunctions.hs Options.hs Common.hs
	ghc Main.hs -o idlewild-lang -DWINDOWS=1 ${GHC_WINDOWS_FLAGS}

libkoshka.core.dll: libkoshka_core.obj
	x86_64-w64-mingw32-gcc -shared -o libkoshka.core.dll libkoshka_core.obj -L C:/Windows/System32 -lmsvcrt -lglib-2.0-0 ${GCC_WINDOWS_FLAGS}

libkoshka_core.obj: libkoshka_core.c
	x86_64-w64-mingw32-gcc -c libkoshka_core.c -DWINDOWS=1 -DUNICODE -D_UNICODE -o libkoshka_core.obj -fpic ${GCC_WINDOWS_FLAGS}

libkoshka.mm.dll: libkoshka_mm.obj libkoshka_mm_graphics.obj libkoshka_mm_sound.obj libkoshka_mm_io.obj libkoshka_mm_text.obj libkoshka.core.dll
	x86_64-w64-mingw32-g++ -shared -o libkoshka.mm.dll libkoshka_mm.obj libkoshka_mm_graphics.obj libkoshka_mm_sound.obj libkoshka_mm_io.obj libkoshka_mm_text.obj -L. -lkoshka.core -lSDL2 -lSDL2_image -lSDL2_ttf -lSDL2_mixer -L C:/Windows/System32 -lOpenGL32 -lfreeglut -lglew32 -lmsvcrt -lglib-2.0-0 ${GCC_WINDOWS_FLAGS}

libkoshka_mm.obj: libkoshka_mm.c
	x86_64-w64-mingw32-gcc -c libkoshka_mm.c -DWINDOWS=1 -o libkoshka_mm.obj -fpic ${GCC_WINDOWS_FLAGS}

libkoshka_mm_graphics.obj: libkoshka_mm_graphics.cpp libkoshka_mm.h
	x86_64-w64-mingw32-g++ -c libkoshka_mm_graphics.cpp -DWINDOWS=1 -o libkoshka_mm_graphics.obj -fpic ${GCC_WINDOWS_FLAGS}

libkoshka_mm_sound.obj: libkoshka_mm_sound.c
	x86_64-w64-mingw32-gcc -c libkoshka_mm_sound.c -DWINDOWS=1 -o libkoshka_mm_sound.obj -fpic ${GCC_WINDOWS_FLAGS}

libkoshka_mm_io.obj: libkoshka_mm_io.c
	x86_64-w64-mingw32-gcc -c libkoshka_mm_io.c -DWINDOWS=1 -o libkoshka_mm_io.obj -fpic ${GCC_WINDOWS_FLAGS}

libkoshka_mm_text.obj: libkoshka_mm_text.c libkoshka_mm.h
	x86_64-w64-mingw32-gcc -c libkoshka_mm_text.c -DWINDOWS=1 -o libkoshka_mm_text.obj -fpic ${GCC_WINDOWS_FLAGS}

install-linux:
	cp idlewild-lang /usr/bin
	cp libkoshka.core.so /usr/lib
	cp libkoshka.mm.so /usr/lib

uninstall-linux:
	-rm -rf /usr/bin/idlewild-lang
	-rm -rf /usr/lib/libkoshka.core.so
	-rm -rf /usr/lib/libkoshka.mm.so

install-windows:
	-mkdir /cygdrive/c/Program\ Files/Idlewild-Lang/
	cp idlewild-lang.exe /cygdrive/c/Program\ Files/Idlewild-Lang/
	cp libkoshka.core.dll /cygdrive/c/Windows/System32/
	cp libkoshka.mm.dll /cygdrive/c/Windows/System32/

uninstall-windows:
	-rm -rf /cygdrive/c/Program\ Files/Idlewild-Lang/
	-rm -rf /cygdrive/c/Windows/System32/libkoshka.core.dll
	-rm -rf /cygdrive/c/Windows/System32/libkoshka.mm.dll

clean:
	-rm -rf *.o *.obj libkoshka.core.so libkoshka.core.dll libkoshka.mm.so libkoshka.mm.dll *.hi idlewild-lang idlewild-lang.exe
