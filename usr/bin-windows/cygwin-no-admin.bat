@REM Run Cygwin setup without admin
@echo off

C:
@REM Change to the directory where Cygwin is installed by scoop
chdir %USERPROFILE%\scoop\apps\cygwin\current

@REM Run Cygwin with -B no admin and
@REM -R set root directory and -l packages directory in scoop persist directory
cygwin-setup.exe -B -R %USERPROFILE%\scoop\persist\cygwin\root -l %USERPROFILE%\scoop\persist\cygwin\packages
