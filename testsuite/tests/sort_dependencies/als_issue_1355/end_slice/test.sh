#!/bin/sh

sort_dependencies -P default.gpr -S main.adb --start-line 2 --start-column 2 --end-line 5 --end-column 4 --no-separator
