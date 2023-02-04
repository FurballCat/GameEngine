@echo off
echo Creating code projects for Furball Cat Game Engine

cd intermediate
cmake ..

cd ..
config\remove_all_build.bat intermediate\GameEngine.sln
