#!/bin/bash
#
# The script install hammerspoon in macOS!
#
# ChangeDate: 2019/3/07
# Author Mephis Pheies <mephistommm@gmail.com>
#
CONFIGURATION_DIR=~/.hammerspoon

if [[ -d $CONFIGURATION_DIR ]]; then
    echo "Hammerspoon configuration has existed!"
    exit 1
fi

git clone https://github.com/MephistoMMM/hammerspoon-config $CONFIGURATION_DIR

echo "Hammerspoon configuration Install Success!"
echo "---"
echo "if you have not installed hammerspoon, you should download it from:"
echo "https://github.com/Hammerspoon/hammerspoon/releases"
echo ""
echo "Have Fun!"
