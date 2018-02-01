#!/bin/bash
#
# The script install emacs in Ubuntu16.04!
#
# Author Mephis Pheies
# Email mephistommm@gmail.com

add-apt-repository ppa:kelleyk/emacs
apt-get update
apt-get install emacs26

${PWD%/scripts}/scripts/link_spacemacs.sh
