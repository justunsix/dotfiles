#!/usr/bin/env bash

# Install LSPs, formatters, linters for use
# with helix editor

scoop install main/marksman
npm i -g prettier
pipx install pyright ruff black
