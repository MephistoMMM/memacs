#!/bin/bash
#
# Author Mephis Pheies
# Email mephistommm@gmail.com

# * Destination
MEMACS_PATH=~/.emacs.d
LATEXMK_PATH=~/.latexmkrc

# * Source
LATEXMK_FILE=latexmkrc

# * Root Dir
CONF_ROOT=${PWD%/scripts}

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
echo "Link $LATEXMK_FILE to $LATEXMK_PATH"
ln -s $CONF_ROOT/$LATEXMK_FILE $LATEXMK_PATH
