@echo off

if [%1]==[] goto usage
@echo Commiting with message
git add . && git commit -m%1 && git push
goto :eof
:usage
@echo Commiting with autocommit
git add . && git commit -m"auto commit script" && git push
exit /B 1
