#!/usr/bin/env bash

# Unstinall my cargo packages
cargo install --list | awk '{print $1}' | xargs -I{} cargo uninstall {}

# Uninstall cargo and rust
rustup self uninstall
