#!/bin/bash
# verify quine is self-consistent (diff on output)

fname="$@"
runghc "$fname" | diff "$fname" -
