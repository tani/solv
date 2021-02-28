#!/bin/bash
# This file is part of the solv distribution (https://github.com/tani/solv).
# Copyright (c) 2021 TANIGUCHI Masaya.
# 
# This program is free software: you can redistribute it and/or modify  
# it under the terms of the GNU General Public License as published by  
# the Free Software Foundation, version 3.
#
# This program is distributed in the hope that it will be useful, but 
# WITHOUT ANY WARRANTY; without even the implied warranty of 
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU 
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License 
# along with this program. If not, see <http://www.gnu.org/licenses/>.

DESTINATION=$(mktemp -d)
ENDLINE=$(grep -n -a '^##ENDLINE$' "$0" | sed 's/:.*//')
tail -n +$((ENDLINE + 1)) "$0" | tar -C "$DESTINATION" -zxf - 
"$DESTINATION/src/solv.pl" "$@"
rm -rf "$DESTINATION"
exit
##ENDLINE
