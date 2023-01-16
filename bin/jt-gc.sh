#!/usr/bin/env bash

# Git Commit and Git Push
# if argument is empty, commit all files and push upstream
# otherwise:
# - Argument must be in format "string" where "string" is the commit message
# - Commit all files and push upstream using message
if [ -z "$1" ]
then
        echo "Commiting with autocommit"
        git add . && git commit -m"auto commit script" && git push
else
        echo "Committing with: " + "$1"
        git add . && git commit -m"$1" && git push
fi