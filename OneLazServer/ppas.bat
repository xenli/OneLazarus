@echo off
SET THEFILE=oneserver
echo Assembling %THEFILE%
D:\devTool\myLazarus\fpc\bin\x86_64-win64\as.exe --64 -o D:\devTool\myLazarus\projects\OneLaz\OneServer\lib\x86_64-win64\OneServer.o   D:\devTool\myLazarus\projects\OneLaz\OneServer\lib\x86_64-win64\OneServer.s
if errorlevel 1 goto asmend
Del D:\devTool\myLazarus\projects\OneLaz\OneServer\lib\x86_64-win64\OneServer.s
SET THEFILE=D:\devTool\myLazarus\projects\OneLaz\OneServer\OneServer.exe
echo Linking %THEFILE%
D:\devTool\myLazarus\fpc\bin\x86_64-win64\ld.exe -b pei-x86-64  --gc-sections   --subsystem windows --entry=_WinMainCRTStartup    -o D:\devTool\myLazarus\projects\OneLaz\OneServer\OneServer.exe D:\devTool\myLazarus\projects\OneLaz\OneServer\link9740.res
if errorlevel 1 goto linkend
goto end
:asmend
echo An error occurred while assembling %THEFILE%
goto end
:linkend
echo An error occurred while linking %THEFILE%
:end
