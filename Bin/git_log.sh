#!/bin/bash
log=$(git log -1 --pretty="%h %cD" )
init=$(git log -1 --pretty=format:'%an' | awk '{for(i=1;i<=NF;i++) printf "%s", substr($i,1,1); print ""}')
echo "'"'"'$log $init'"'"'"
