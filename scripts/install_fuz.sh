#!/bin/bash
# install fuz and  fuz.el
#
# Author: Mephis Pheies <mephistommm@gmail.com>
# Date: 2019/08/03

FUZ_EL_DIR_NAME=fuz.el
FUZ_EL_INSTALLED_DIR=~/.local/share
FUZ_EL_PATH=$FUZ_EL_INSTALLED_DIR/$FUZ_EL_DIR_NAME

# prepare rust and cargo
if type cargo > /dev/null 2>&1; then
    echo "Rust has been installed."
else
    brew install rust
    if [[ $? -ne 0 ]]; then
        echo "Failed to install rust!"
        exit 1
    fi
fi

# init directory and clone project
mkdir -p $FUZ_EL_INSTALLED_DIR
cd $FUZ_EL_INSTALLED_DIR
if [[ ! -d $FUZ_EL_PATH ]]; then
    git clone https://github.com/cireu/fuz.el.git $FUZ_EL_DIR_NAME
    if [[ $? -ne 0 ]]; then
        echo "Failed to clone fuz.el from github!"
        exit 1
    fi
fi

# compile fuz
cd $FUZ_EL_PATH
cargo build --release
if [[ $? -ne 0 ]]; then
    echo "Failed to compile fuz!"
    exit 1
fi
ln -sv target/release/libfuz_core.dylib fuz-core.so
if [[ $? -ne 0 ]]; then
    echo "Failed to link fuz-core.so!"
    exit 1
fi
echo "Success to install fuz.el!"
