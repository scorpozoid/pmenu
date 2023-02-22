rem @echo off

set CMDDIR=
set CMDFILE=%~f0
for /f "delims=" %%i in ("%CMDFILE%") do set CMDDIR=%%~di%%~pi
cd %CMDDIR%

if exist %CMDDIR%example.pmenu %CMDDIR%pmenu.exe %CMDDIR%example.pmenu


rem E O F
