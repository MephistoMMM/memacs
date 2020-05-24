#!/bin/bash
#
# The script compile textinpurtsource in macOS!
#
# Author Mephis Pheies
# Email mephistommm@gmail.com

ROOTPATH=${PWD%/scripts}

# compile ./local/textinputsource.c then mv to ~/.local/bin
if [ -d $HOME/.local/bin ]; then
    echo "$HOME/.local/bin exists."
else
    echo "Create $HOME/.local/bin."
    mkdir -p $HOME/.local/bin
fi
gcc -framework Carbon -o $HOME/.local/bin/textinputsource $ROOTPATH/lib/text_input_source/main.c
