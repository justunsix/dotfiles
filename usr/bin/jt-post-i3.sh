#!/usr/bin/env bash

# Post i3 Start up Script
# - Run Redshift if it is not already running and program is present
# - Adjust displays if the number connected are more than 1

# if redshift program exists
if [ -x "$(command -v redshift)" ]; then
		# if redshift is running
		if pgrep -x "redshift" > /dev/null
		then
				# kill redshift
				killall redshift
		fi
		# start redshift
		redshift -t 2200:2200 -m randr -l 43:79 &
		
fi

# if multiple displays are detected
if [ $(xrandr | grep -c " connected") -gt 1 ]; then
		# if xrandr program exists
		if [ -x "$(command -v xrandr)" ]; then
				# run screen configuration
  		  # Rotate DP-4 screen to portrait
				xrandr --output DP-4 --rotate left
				xrandr --output DP-2 --primary
		fi
fi
