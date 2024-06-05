#!/bin/sh

sort_dependencies -P default.gpr -S main.adb --start-line 2 --start-column 2 --end-line 2 --end-column 2 --no-separator
