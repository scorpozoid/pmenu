@echo off

set PMENU_VER=0.01

set USERBIN=C:\home\%USERNAME%\bin
set PATH=%USERBIN%\7z-22.01\App\7-Zip
set PATH=%USERBIN%\lazarus-2.2.4;%PATH%
set PATH=%USERBIN%\lazarus-2.2.4\fpc\3.2.2\bin\i386-win32;%PATH%

set CMDDIR=
set CMDFILE=%~f0
for /f "delims=" %%i in ("%CMDFILE%") do set CMDDIR=%%~di%%~pi
cd %CMDDIR%

if exist pmenu.exe del /s /q pmenu.exe

> .\pmenu.version echo Result ^:= '%PMENU_VER%';

lazbuild.exe --build-all --recursive pmenu.lpi
strip.exe pmenu.exe

> .\pmenu.version echo Result ^:= '';

rem for /f %%i in ('pmenu.exe --version') do set PMENU_VER=%%i

set PMENU_Z=pmenu-%PMENU_VER%.7z

if exist %PMENU_Z% del %PMENU_Z%
if exist bin rmdir /s /q bin

mkdir bin
copy pmenu.exe bin
copy example.pmenu bin
7z a %PMENU_Z% bin

if exist bin rmdir /s /q bin

pause

rem E O F
