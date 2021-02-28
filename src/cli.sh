#!/bin/bash
DESTINATION=$(mktemp -d)
ENDLINE=$(grep -n -a '^##ENDLINE$' "$0" | sed 's/:.*//')
tail -n +$((ENDLINE + 1)) "$0" | tar -C "$DESTINATION" -zxf - 
"$DESTINATION/src/solv.pl" "$@"
rm -rf "$DESTINATION"
exit
##ENDLINE
