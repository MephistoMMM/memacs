#!/bin/bash
#
# The script install emacs in macOS!
#
# Author Mephis Pheies
# Email mephistommm@gmail.com

ROOTPATH=${PWD%/scripts}

brew tap railwaycat/emacsmacport
brew install emacs-mac
brew link emacs-mac
ln -s /usr/local/opt/emacs-mac/Emacs.app /Applications

$ROOTPATH/scripts/compile_textinputsource.sh
$ROOTPATH/scripts/link_memacs.sh
