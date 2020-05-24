#!/bin/bash
#
# Author Mephis Pheies
# Email mephistommm@gmail.com

# * Ready Bin Dir
BIN_DIR=~/.local/bin
if [ "$1" ]; then
    BIN_DIR=$1
fi

if [ ! -e $BIN_DIR ]; then
    mkdir -p $BIN_DIR
fi

# * Tools Dir
TOOLS_ROOT=${PWD%/scripts}/bin

for script in $TOOLS_ROOT/*.sh; do
    if [ -x $script ]; then
        suffix=${script##/*/bin/}
        echo "Link $script to $BIN_DIR/${suffix%.sh}"
        ln -s $script $BIN_DIR/${suffix%.sh}
    fi
done
