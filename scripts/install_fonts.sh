#!/bin/bash
#
# The script init powerlinefonts!
#
# ChangeDate: 2018/9/21
# Author Mephis Pheies <mephistommm@gmail.com>
#
ROOTPATH=${PWD%/scripts}

echo "Start install Nerd Icons fonts ..."
brew install font-hack-nerd-font
if [ $? -eq 0 ]; then
    echo "Finish installing Nerd Icons fonts."
else
    echo "Failed to install Nerd Icons fonts!"
fi

echo "Start install Fira Code Memacs Symbol fonts..."
# https://github.com/tonsky/FiraCode/wiki/Installing
brew install font-fira-code
if [ $? -eq 0 ]; then
    echo "Finish installing Fira Code Memacs Symbol fonts."
else
    echo "Failed to install Fira Code Memacs Symbol fonts!"
fi
