#!/bin/bash
# propagate internal changes if they are self-consistent, keep the output if not

fname="$@"
runghc "$fname" > temp && runghc temp | diff temp - && mv temp "$fname"
