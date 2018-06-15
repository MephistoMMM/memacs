#!/bin/bash
#
# The script install emacs in macOS!
#
# Author Mephis Pheies
# Email mephistommm@gmail.com

brew tap railwaycat/emacsmacport/emacs-mac
brew install emacs-mac
brew linkapps emacs-mac

${PWD%/scripts}/scripts/link_spacemacs.sh
