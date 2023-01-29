@echo off

@REM Loop through all folders at current directory and run git status on them
@REM to find which folders have changes
pushd .
for /D %%i in (".\*") do call :$gits "%%i"
popd

@REM pause
exit /B

::**************************************************
:$gits
::**************************************************

cd %1

@REM Check if folder is a git repository, if so, run git status, otherwise skip
if exist .git (

    @REM Run git status and check output, 
    @REM if output contains "nothing to commit, working tree clean", then echo "."
    @REM Else print the working directory
    git status | findstr /c:"nothing to commit, working tree clean" > nul && echo . || echo %1 has changes
) 
cd ..

exit /B