@REM Open Cygwin bash shell from Windows Command Line

@REM Use directory where Cygwin is installed by scoop
%USERPROFILE%\scoop\apps\cygwin\current\root\bin\bash.exe --login -i -c "export CD='%CD%'; cd \"$CD\"; exec /bin/bash"
