#!/bin/bash 
if [ "$3" = "" ]; then
    echo "Splitting a file by lines"
    echo "Usage: $0 <filename> <parts> <output_directory>"
else
    split -l $2 $1 > /dev/null
    ls -a | grep "^x[a-z|A-Z]" | ( while read FILES ; do mv "$FILES" "$3"; echo "NEW_SPLIT $FILES" ; done)
fi

