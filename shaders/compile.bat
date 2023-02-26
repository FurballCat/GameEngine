@echo off

setlocal EnableDelayedExpansion

:: Get the path to the Vulkan SDK from the environment variable
set VULKAN_SDK_PATH=%VULKAN_SDK%

:: Change the current directory to the location of the batch file
cd /d "%~dp0"

echo.
echo ## Start shader compilation ##
echo.

:: Create the output directory if it does not exist
if not exist compiled mkdir compiled

:: Compile vertex shaders
for %%f in (*.vert) do (
    "%VULKAN_SDK_PATH%\Bin\glslangValidator.exe" -V "%%f" -o "compiled\%%~nf.spv"
)

:: Compile fragment shaders
for %%f in (*.frag) do (
    "%VULKAN_SDK_PATH%\Bin\glslangValidator.exe" -V "%%f" -o "compiled\%%~nf.spv"
)

echo.
echo ## compilation finished ##
echo.
pause