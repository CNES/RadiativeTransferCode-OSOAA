@echo on
cls

title OSOAA_UI
setlocal

if defined OSOAA_ROOT goto OSOAAMAIN

REM You can set here OSOAA_ROOT environment variable.
set OSOAA_ROOT=%CD%

:OSOAAMAIN

REM -- root directory for the project
set CURDIR=%CD%

start javaw -Dlog4j.configuration=file:%OSOAA_ROOT%/log4j.properties -jar OSOAAUI.jar osoaa.Launcher

cd %CURDIR%

endlocal
