#!/bin/bash
SCRIPT_PATH=${0%/*}
H="-h"

if [ "$0" != "$SCRIPT_PATH" ] && [ "$SCRIPT_PATH" != "" ]; then 
    cd $SCRIPT_PATH
fi

if [ -z "$1" ] || [ "$1" == "-h" ]; then
    echo "Usage: ./render.sh -template template.html -target target-dir
    -i markdown1.cd markdown2.cd ... | -d path/to/markdowns-target
    [-history (not working rn)]"
    else
        lein run ${@:1}
fi