@echo off

python.exe --version >NUL 2>&1
if errorlevel 1 goto error
goto buildsolution

:ERROR
echo Python.exe not found!
goto endpause

:BUILDSOLUTION
python "projectBuilder/main.py" -update
goto end

:ENDPAUSE
pause

:END
