#!/bin/bash
#
# The script install emacs in Ubuntu16.04!
#
# Author Mephis Pheies
# Email mephistommm@gmail.com

mkdir -p tmp/emacs && cd tmp/emacs
git init
git remote add origin https://github.com/emacs-mirror/emacs.git
git fetch --depth 1 origin emacs-26
git reset --hard FETCH_HEAD
sudo apt install autoconf make gcc texinfo libgtk-3-dev libxpm-dev libjpeg-dev libgif-dev libtiff5-dev libgnutls-dev libncurses5-dev
./configure
make
sudo make install
