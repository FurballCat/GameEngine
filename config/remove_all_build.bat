@echo off

set slnfile="%1"
set tempfile="%temp%\temp.sln"

if not exist %slnfile% (
  echo Solution file not found: %slnfile%
  pause
  exit /b 1
)

echo Processing solution file: %slnfile%

type %slnfile% | findstr /v "ALL_BUILD" | findstr /v "CMakePredefinedTargets" > %tempfile%

if exist %tempfile% (
  echo Temporary file created: %tempfile%
  move /y %tempfile% %slnfile% > nul
  echo Solution file updated: %slnfile%
) else (
  echo Failed to create temporary file: %tempfile%
)

exit /b 0