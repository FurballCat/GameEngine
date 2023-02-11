@echo off
echo Creating code projects for Furball Cat Game Engine

:: create bin folder
if not exist bin mkdir bin

:: create bin/lib folder
cd bin
if not exist lib mkdir lib
cd ..

:: create intermediate folder
if not exist intermediate mkdir intermediate

:: run cmake and generate stuff in intermediate folder
cd intermediate
cmake ..

:: remove the ALL_BUILD project from .sln file manually
echo Running custom Furball Cat post project generation scripts

cd ..

:: add MASM assembler for .asm and fcontext files in ccore project
:: call config\add_masm.bat intermediate\projects\ccore\ccore.vcxproj

:: remove ALL_BUILD from projects in solution
call config\remove_all_build.bat intermediate\GameEngine.sln


exit /b 0
