# Copy HTML from clipboard and convert to Emacs org format and set back to clipboard
# Switches:
# --wrap=none : don't wrap lines
Get-Clipboard | pandoc -f html -t org --wrap=none | Set-Clipboard
