#!/bin/bash
#
# A script to install Microsoft python language server in macOS.
#
# Author: Mephis Pheies <mephistommm@gmail.com>
# Create Date: 2019/07/06

LSP_PYTHON_MS_DIR=$HOME/.local/share/lsp_python_ms
LSP_PYTHON_MS_IMPL_DIR=$LSP_PYTHON_MS_DIR/src/LanguageServer/Impl
LSP_PYTHON_MS_RELEASE_DIR=$LSP_PYTHON_MS_DIR/output/bin/Release
GIT_CLONE_DIR=$LSP_PYTHON_MS_DIR

if [[ -d $LSP_PYTHON_MS_RELEASE_DIR ]]; then
    echo "lsp-python-ms has been installed."
    exit 0
fi

echo "Start to install lsp-python-ms..."

if [[ ! -d $LSP_PYTHON_MS_DIR ]]; then
    echo "    Not found source code of lsp-python-ms."
    echo "    Clone source code..."
    git clone --depth=1 \
        https://github.com/Microsoft/python-language-server.git \
        $LSP_PYTHON_MS_DIR
    if [[ ! $? -eq 0 ]]; then
        echo "Failed to clone source code from github."
        exit 1
    fi
    echo "    Done"
fi

if type dotnet > /dev/null 2>&1; then
    echo "    .Net Core SDK has been installed."
else
    echo "    Not found .Net Core SDK: 'dotnet'."
    echo "    Install dotnet-sdk..."
    brew cask install dotnet-sdk
    if [[ ! $? -eq 0 ]]; then
        echo "Failed to install dotnet-sdk."
        exit 1
    fi

    ln -s /usr/local/share/dotnet/dotnet /usr/local/bin/dotnet
    echo "    Done"
fi

echo "    Build lsp-python-ms.."
cd $LSP_PYTHON_MS_IMPL_DIR
dotnet build -c Release && dotnet publish -c Release -r osx-x64
if [[ ! $? -eq 0 ]]; then
    echo "Failed to Build lsp-python-ms."
    exit 1
fi

echo "Done."
