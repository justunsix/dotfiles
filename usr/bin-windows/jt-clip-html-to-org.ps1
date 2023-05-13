#!/usr/bin/env bash

# Copy HTML from clipboard and convert to Emacs org format and set back to clipboard
# Switches:
# --wrap=none : don't wrap lines

$clip = Get-Clipboard -Format Html
# $pandoc = "C:\Program Files\Pandoc\pandoc.exe"
$org = pandoc -f html -t org --wrap=none $clip
Set-Clipboard -Value $org -Format Text
