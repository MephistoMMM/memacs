#!/bin/bash
#
# The script install emacs in macOS!
#
# Author Mephis Pheies
# Email mephistommm@gmail.com

brew tap d12frosted/emacs-plus
brew install emacs-plus
brew linkapps emacs-plus

${PWD%/scripts}/scripts/link_spacemacs.sh
