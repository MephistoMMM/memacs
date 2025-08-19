#!/bin/bash
#
# The script init powerlinefonts!
#
# ChangeDate: 2018/9/21
# Author Mephis Pheies <mephistommm@gmail.com>
#
ROOTPATH=${PWD%/scripts}

echo "Start install powerline fonts..."
export FONTS_PATH=/tmp/memacs_`date "+%Y%m%d"`/install_fonts

if [ ! -d $FONTS_PATH ]; then
    mkdir -p $FONTS_PATH
fi

git clone https://github.com/powerline/fonts.git $FONTS_PATH/powerlinefonts \
        && $FONTS_PATH/powerlinefonts/install.sh
if [ $? -eq 0 ]; then
    echo "Finish installing powerline fonts."
else
    echo "Failed to install powerline fonts!"
fi

echo "Start install Fira Code Memacs Symbol fonts..."
# https://github.com/tonsky/FiraCode/wiki/Installing
brew tap homebrew/cask-fonts && brew cask install font-fira-code
if [ $? -eq 0 ]; then
    echo "Finish installing Fira Code Memacs Symbol fonts."
else
    echo "Failed to install Fira Code Memacs Symbol fonts!"
fi
