#!/usr/bin/env bash

# Copy HTML from clipboard and convert to Emacs org format and set back to clipboard
# for X, if using Wayland use wl-copy instead of xclip
# Switches:
# -selection : fix pasting html to terminal
# per https://askubuntu.com/questions/1397056/work-around-for-xclip-o-selection-clipboard-t-text-html-error-target-t
# --wrap=none : don't wrap lines
# sed... : remove backslashes from line indicators added by pandoc
xclip -o -selection clipboard -t text/html | pandoc -f html -t org --wrap=none | sed 's/\\\\//g' | xclip -selection clipboard
