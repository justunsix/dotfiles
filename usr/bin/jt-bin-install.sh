#!/usr/bin/env bash

# Install binaries of certain programs
#
# Some programs may be more secure, easier, or faster to install
# from the provider's binaries instead of through package managers

# Run all scripts in programs/
for f in programs/*.sh; do bash "$f" -H; done