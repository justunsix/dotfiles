@echo off

@REM Loop through all folders at current directory and run git pull on them
pushd .
for /D %%i in (".\*") do call :$gitsync "%%i"
popd

@REM pause
exit /B

::**************************************************
:$gitsync
::**************************************************

cd %1

@REM Check if folder is a git repository, if so, run git pull, otherwise skip
if exist .git (
    echo -
    echo ----------------------------------------
    echo Repo:  %1

    git pull
) 
cd ..

exit /B