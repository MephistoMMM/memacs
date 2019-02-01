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
cp $ROOTPATH/local/fonts/FiraCode-Regular-Memacs-Symbol.otf ~/Library/Fonts/FiraCode-Regular-Symbol.otf
if [ $? -eq 0 ]; then
    echo "Finish installing Fira Code Memacs Symbol fonts."
else
    echo "Failed to install Fira Code Memacs Symbol fonts!"
fi

# https://github.com/sebastiencs/icons-in-terminal
echo "Start install icons-in-terminal fonts..."
cp $ROOTPATH/local/fonts/icons-in-terminal.ttf ~/Library/Fonts/icons-in-terminal.ttf
if [ $? -eq 0 ]; then
    echo "Finish installing icons-in-terminal fonts."
else
    echo "Failed to install icons-in-terminal fonts!"
fi
