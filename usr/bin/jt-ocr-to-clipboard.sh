#!/usr/bin/env bash

# Ideas per https://willbush.dev/blog/fast-ocr-to-clipboard/

# Check if Wayland is running

# If using Wayland use wl-copy instead of xclip
if [ -z "$WAYLAND_DISPLAY" ]; then
		# on X11
		# option using xclip
		# flameshot gui --raw | tesseract stdin stdout -l eng | xclip -in -selection clipboard
		# option using xsel
		flameshot gui --raw | tesseract stdin stdout -l eng | xsel --input --clipboard
else
		# Wayland
		flameshot gui --raw | tesseract stdin stdout -l eng | wl-copy	
fi
