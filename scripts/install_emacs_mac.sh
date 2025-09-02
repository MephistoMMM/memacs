#!/bin/bash
#
# The script install emacs in macOS!
#
# ChangeDate: 2018/9/21
# Author Mephis Pheies <mephistommm@gmail.com>
#

ROOTPATH=${PWD%/scripts}

brew tap railwaycat/emacsmacport
brew install emacs-mac --with-starter --with-dbus --with-glib --with-imagemagick --with-librsvg
brew link emacs-mac
ln -s /usr/local/opt/emacs-mac/Emacs.app /Applications

# install powerline fonts
$ROOTPATH/scripts/install_fonts.sh
# install testinputsource -- change to more better plan
# $ROOTPATH/scripts/compile_textinputsource.sh
# use smart input source, rather than textinputsource
brew tap laishulu/homebrew
brew install macism
# install hammerspoon
# $ROOTPATH/scripts/install_hammerspoon.sh
# link emacs
$ROOTPATH/scripts/link_memacs.sh
# link tools
$ROOTPATH/scripts/link_tools.sh
