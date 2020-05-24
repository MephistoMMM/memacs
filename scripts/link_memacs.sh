#!/bin/bash
#
# Author Mephis Pheies
# Email mephistommm@gmail.com

# * Destination
MEMACS_PATH=~/.emacs.d
LATEXMK_PATH=~/.latexmkrc

# * Root Dir
CONF_ROOT=${PWD%/scripts}

# * Source
LATEXMK_FILE=latexmkrc

# * Check Files Exist And Delete

if [ -e $MEMACS_PATH ]; then
    echo "Remove $MEMACS_PATH"
    rm -rf $MEMACS_PATH
fi

if [ -e $LATEXMK_PATH ]; then
    echo "Remove $LATEXMK_PATH"
    rm -rf $LATEXMK_PATH
fi

# * Create Soft Link

echo "Link $CONF_ROOT to $MEMACS_PATH"
ln -s $CONF_ROOT $MEMACS_PATH
echo "Link ./lib/$LATEXMK_FILE to $LATEXMK_PATH"
ln -s $CONF_ROOT/lib/$LATEXMK_FILE $LATEXMK_PATH
