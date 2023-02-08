@echo off

echo Clearing Fubrall Cat Game Engine temp folders

if exist "bin" (
    echo Deleting bin folder...
    rd /s /q "bin"
)

if exist "intermediate" (
    echo Deleting intermediate folder...
    rd /s /q "intermediate"
)

echo Done.