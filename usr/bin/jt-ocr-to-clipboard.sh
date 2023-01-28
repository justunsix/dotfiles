#!/usr/bin/env bash

# Ideas per https://willbush.dev/blog/fast-ocr-to-clipboard/

# Check if Wayland is running

# If using Wayland use wl-copy instead of xclip
if [ -z "$WAYLAND_DISPLAY" ]; then
		# on X11
		flameshot gui --raw | tesseract stdin stdout -l eng | xclip -in -selection clipboard
else
		# Wayland
		flameshot gui --raw | tesseract stdin stdout -l eng | wl-copy	
fi
