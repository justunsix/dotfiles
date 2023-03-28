#!/usr/bin/env bash

# Copy HTML from clipboard and convert to Emacs org format and set back to clipboard
# for X, if using Wayland use wl-copy instead of xclip
# Switches:
# --wrap=none : don't wrap lines
# sed... : remove backslashes from line indicators added by pandoc
xclip -o -t text/html | pandoc -f html -t org --wrap=none | sed 's/\\\\//g' | xclip -selection clipboard
