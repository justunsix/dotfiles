@echo off

if [%1]==[] goto usage
@echo Commiting with message
git commit -am%1 && git push
goto :eof
:usage
@echo Commiting with autocommit
git commit -am"auto commit script" && git push
exit /B 1
