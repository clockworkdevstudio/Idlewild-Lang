copy %~dp0..\idlewild-lang-dependencies\* %cd%
rmdir %1 /S /Q
del %1.zip
mkdir %1
mkdir %1\Examples
copy idlewild-lang.exe %1
copy *.dll %1
copy FASM.EXE %1
copy nasm.exe %1
copy install-windows-bin.bat %1
copy LICENSE* %1
copy GoLink.exe %1
xcopy Examples %1\Examples /E
7z.exe a -r %1.zip %1