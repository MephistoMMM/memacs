#!/usr/bin/env bash
##
# Launch emacs from emacs.d folder you placed anywhere.
#
# @author gongqijian@gmail.com
# @date 2016-07-21
#

function usage {
    cat<<EOF
Usage: $(basename $0) [emacs_path] [options]

Emacs path:
    Path of emacs executable file, default is $(which emacs)

Options:
    -h | --help     Print help
    -l | --lang     LANG environment variable, default is en_US.UTF-8
    -t | --term     TERM environment variable, default is xterm
    -a | --argv     Arguments of emacs, default is '-nw --debug-init'

Example:
    git clone https://github.com/purcell/emacs.d emacs.d-purcell
    cd emacs.d-purcell
    $(basename $0) /Applications/Emacs.app/Contents/MacOS/Emacs -a '-nw --debug-init'
EOF
}

home_fake=$PWD
emacs_bin=emacs
opt_term=xterm
opt_lang=en_US.utf-8
opt_argv='-nw --debug-init'

while [[ $# -gt 0 ]]; do
    case $1 in
        -*)
            case $1 in
                -h | --help) usage; exit 0;;
                -l | --lang) shift; opt_lang=$1;;
                -t | --term) shift; opt_term=$1;;
                -a | --argv) shift; opt_argv=$1;;
                *) echo Unknown option $1; usage; exit 1;;
            esac
            ;;
        *)
            emacs_bin=$1
            ;;
    esac
    shift
done

if [[ ! -e init.el ]]; then
    cat <<EOF
init.el not found!
Please run $(basename $0) in an emacs.d folder.
EOF
    exit 1
fi

if [[ ! -e ./.emacs.d ]]; then
    ln -s . ./.emacs.d
fi

if [[ -e ~/.terminfo && ! -e ./.terminfo ]]; then
    ln -snf ~/.terminfo ./.terminfo
fi

case $(uname -s) in
    Darwin) ln -sf ~/Library ./Library
esac

if [[ -d .git ]]; then
    mkdir -p .git/info
    touch .git/info/exclude
    if [[ -z $(grep -E '^\.emacs\.d$' .git/info/exclude) ]]; then
        echo '.emacs.d' >> .git/info/exclude
    fi
    if [[ -z $(grep -E '^Library$' .git/info/exclude) ]]; then
        echo 'Library' >> .git/info/exclude
    fi
fi

export TERM=$opt_term
export LANG=$opt_lang
export HOME=$home_fake

"$emacs_bin" $opt_argv
