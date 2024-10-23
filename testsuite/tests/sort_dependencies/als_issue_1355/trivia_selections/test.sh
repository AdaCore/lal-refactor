#!/bin/sh

sort_dependencies -P default.gpr -S main.adb --start-line 1 --start-column 1 --end-line 8 --end-column 1 --no-separator
