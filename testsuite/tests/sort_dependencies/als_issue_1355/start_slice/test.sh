#!/bin/sh

sort_dependencies -P default.gpr -S main.adb --start-line 1 --start-column 2 --end-line 4 --end-column 4 --no-separator
