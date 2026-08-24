@echo off
rem ====================================================================
rem  CFU Plot Studio - double-click to run.
rem
rem  Installs anything missing into a private library beside this file on
rem  first run, then opens the app in your browser. Nothing is installed
rem  system-wide and nothing leaves your machine.
rem
rem  Finds R in this order (first hit wins):
rem    1. CFU_RSCRIPT env var (point it at Rscript.exe to force a choice)
rem    2. Rscript on PATH
rem    3. the install path R recorded in the registry
rem    4. common install locations, newest version first
rem ====================================================================
setlocal EnableDelayedExpansion
cd /d "%~dp0"

set "RSCRIPT="

rem --- 1. explicit override -------------------------------------------------
if defined CFU_RSCRIPT if exist "%CFU_RSCRIPT%" set "RSCRIPT=%CFU_RSCRIPT%"

rem --- 2. Rscript on PATH ---------------------------------------------------
if not defined RSCRIPT (
  for /f "delims=" %%P in ('where Rscript.exe 2^>nul') do (
    if not defined RSCRIPT set "RSCRIPT=%%P"
  )
)

rem --- 3. registry ----------------------------------------------------------
rem  R writes InstallPath under R-core. Check the per-user hive first, then
rem  machine-wide, and each of those in both the native and 32-bit views.
if not defined RSCRIPT (
  for %%H in ("HKCU\SOFTWARE\R-core\R" "HKLM\SOFTWARE\R-core\R" "HKLM\SOFTWARE\WOW6432Node\R-core\R") do (
    if not defined RSCRIPT (
      for /f "tokens=2,*" %%A in ('reg query %%H /v InstallPath 2^>nul ^| findstr /i "InstallPath"') do (
        if not defined RSCRIPT if exist "%%B\bin\Rscript.exe" set "RSCRIPT=%%B\bin\Rscript.exe"
      )
    )
  )
)

rem --- 4. common install locations, newest first ----------------------------
if not defined RSCRIPT (
  for %%R in (
    "%ProgramFiles%\R"
    "%ProgramFiles(x86)%\R"
    "%LOCALAPPDATA%\Programs\R"
    "%USERPROFILE%\scoop\apps\r\current"
  ) do (
    if not defined RSCRIPT if exist "%%~R" (
      for /f "delims=" %%D in ('dir /b /ad /o-n "%%~R\R-*" 2^>nul') do (
        if not defined RSCRIPT if exist "%%~R\%%D\bin\Rscript.exe" set "RSCRIPT=%%~R\%%D\bin\Rscript.exe"
      )
      if not defined RSCRIPT if exist "%%~R\bin\Rscript.exe" set "RSCRIPT=%%~R\bin\Rscript.exe"
    )
  )
)

if not defined RSCRIPT (
  echo.
  echo   Could not find R on this computer.
  echo.
  echo   Install it from  https://cran.r-project.org/bin/windows/base/
  echo   then run this file again.
  echo.
  echo   Already have R somewhere unusual? Set CFU_RSCRIPT to your Rscript.exe
  echo   and run this file again. For example:
  echo       setx CFU_RSCRIPT "C:\Program Files\R\R-4.5.0\bin\Rscript.exe"
  echo.
  pause
  exit /b 1
)

echo Using R: %RSCRIPT%
echo.
echo Starting CFU Plot Studio - your browser will open shortly.
echo Close this window to stop the app.
echo.

"%RSCRIPT%" "%~dp0run_app.R"
if errorlevel 1 goto :failed
goto :eof

:failed
echo.
echo   The app stopped with an error - see the messages above.
echo.
echo   First run and it failed while installing packages? Check your internet
echo   connection. Behind a proxy, set HTTPS_PROXY and try again.
echo.
pause
exit /b 1
