#!/bin/bash
#
# The script install emacs in macOS!
#
# ChangeDate: 2018/9/21
# Author Mephis Pheies <mephistommm@gmail.com>
#

ROOTPATH=${PWD%/scripts}

brew tap railwaycat/emacsmacport
brew install emacs-mac
brew link emacs-mac
ln -s /usr/local/opt/emacs-mac/Emacs.app /Applications

# install powerline fonts
$ROOTPATH/scripts/install_fonts.sh
# install testinputsource
$ROOTPATH/scripts/compile_textinputsource.sh
# link emacs
$ROOTPATH/scripts/link_memacs.sh
