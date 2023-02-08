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
cd ..
config\remove_all_build.bat intermediate\GameEngine.sln
