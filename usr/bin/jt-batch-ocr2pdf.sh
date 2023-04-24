#!/usr/bin/env bash

# OCR all the image files in a directory
# per https://ocrmypdf.readthedocs.io/en/latest/cookbook.html

for file in *.jpg *.png; do
  echo "$file"
  img2pdf "$file" | ocrmypdf - "$file.pdf" --rotate-pages
done
