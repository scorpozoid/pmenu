@echo off

set CMDDIR=
set CMDFILE=%~f0
for /f "delims=" %%i in ("%CMDFILE%") do set CMDDIR=%%~di%%~pi
cd %CMDDIR%

echo Setup Lazarus and FPC folders paths...
set USERBIN=C:\home\%USERNAME%\bin
set PATH=%USERBIN%\7z-22.01\App\7-Zip;%PATH%
set PATH=%USERBIN%\lazarus-2.2.4;%PATH%
set PATH=%USERBIN%\lazarus-2.2.4\fpc\3.2.2\bin\i386-win32;%PATH%

echo Setup PowerShell paths...
set PATH=C:\Windows\System32\WindowsPowerShell\v1.0;%PATH%
set PATH=C:\Windows\SysWOW64\WindowsPowerShell\v1.0;%PATH%

echo Remove old binary...
if exist pmenu.exe del /s /q pmenu.exe
if exist pmenu-*.7z del /s /q pmenu-*.7z

echo Extrude local timestamp...
for /F "usebackq tokens=1,2 delims==" %%i in (`wmic os get LocalDateTime /VALUE 2^>NUL`) do if '.%%i.'=='.LocalDateTime.' set localtime=%%j
set localtime=%localtime:~0,4%-%localtime:~4,2%-%localtime:~6,2% %localtime:~8,2%:%localtime:~10,2%:%localtime:~12,6%
set timestamp=%localtime%
set timestamp=%timestamp: =%
set timestamp=%timestamp:.=%
set timestamp=%timestamp::=%
set timestamp=%timestamp:/=%
set timestamp=%timestamp:-=%

echo Prepare revision mark...
set PMENU_VER_MAJOR=%timestamp:~0,4%
set PMENU_VER_MINOR=1%timestamp:~4,4%
set PMENU_VER_RELEASE=1%timestamp:~8,4%
set PMENU_VER_BUILD=1%timestamp:~11,4%
set PMENU_VER_VERSION=%PMENU_VER_MAJOR%.%PMENU_VER_MINOR%.%PMENU_VER_RELEASE%.%PMENU_VER_BUILD%
set PMENU_VER=0.1.%timestamp:~2,8%

> .\pmenu.version echo Result ^:= '%PMENU_VER%';

echo Setup revision mark...
powershell.exe -Command "(gc pmenu.lpi) | Foreach-Object {$_ " ^
  "-replace '<MajorVersionNr Value=\".*?\"/>', '<MajorVersionNr Value=\"%PMENU_VER_MAJOR%\"/>'" ^
  "-replace '<MinorVersionNr Value=\".*?\"/>', '<MinorVersionNr Value=\"%PMENU_VER_MINOR%\"/>'" ^
  "-replace '<RevisionNr Value=\".*?\"/>', '<RevisionNr Value=\"%PMENU_VER_RELEASE%\"/>'" ^
  "-replace '<BuildNr Value=\".*?\"/>', '<BuildNr Value=\"%PMENU_VER_BUILD%\"/>'" ^
  "-replace 'ProductVersion=\".*?\"', 'ProductVersion=\"%PMENU_VER%\"'" ^
  "-replace 'FileDescription=\".*?\"', 'FileDescription=\"%PMENU_VER% (%PMENU_VER_VERSION%)\"'" ^
  "} | Set-Content -Encoding UTF8 pmenu.lpi

echo Build...
lazbuild.exe --build-all --recursive pmenu.lpi
strip.exe pmenu.exe

echo Revert...
> .\pmenu.version echo Result ^:= '';

echo Build (portable) install...

set PMENU_N=pmenu-%PMENU_VER%
set PMENU_Z=%PMENU_N%.7z

if exist %PMENU_Z% del %PMENU_Z%
if exist %PMENU_N% rmdir /s /q %PMENU_N%

mkdir %PMENU_N%
copy pmenu.exe      %PMENU_N%
copy pmenu.bat      %PMENU_N%
copy example.pmenu  %PMENU_N%
copy readme.md      %PMENU_N%
7z a %PMENU_Z% %PMENU_N%

if exist %PMENU_N% rmdir /s /q %PMENU_N%

rem E O F
