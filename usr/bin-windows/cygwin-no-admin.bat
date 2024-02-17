@REM Run Cygwin setup without admin
@echo off

C:
@REM Change to the directory where Cygwin is installed by scoop
chdir %USERPROFILE%\scoop\apps\cygwin\current

cygwin-setup.exe -B
