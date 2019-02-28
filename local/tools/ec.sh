#!/bin/bash
# https://github.com/lujun9972/emacs-document/blob/master/emacs-common/我是怎样使用Emacs的.org

# This script starts emacs daemon if it is not running, opens whatever file
# you pass in and changes the focus to emacs.  Without any arguments, it just
# opens the current buffer or *scratch* if nothing else is open.  The following
# example will open ~/.bashrc

# ec ~/.bashrc

# You can also pass it multiple files, it will open them all.  Unbury-buffer
# will cycle through those files in order

# The compliment to the script is et, which opens emacs in the terminal
# attached to a daemon

# If you want to execute elisp, pass in -e whatever.
# You may also want to stop the output from returning to the terminal, like
# ec -e "(message \"Hello\")" > /dev/null

# emacsclient options for reference
# -a "" starts emacs daemon and reattaches
# -c creates a new frame
# -n returns control back to the terminal
# -e eval the script

if [[ ! -e $EMACS_SOCKET_NAME ]]; then
    echo 'Please Config $EMACS_SOCKET_NAME First!'
    exit 1
fi

ps aux | grep 'Emacs' | grep -v 'grep' > /dev/null
if [[ $? -ne 0 ]]; then
    echo "Please Start Emacs Daemon First!"
    exit 1
fi

# Number of current visible frames,
# Emacs daemon always has a visible frame called F1
visible_frames() {
    emacsclient -s $EMACS_SOCKET_NAME -e '(length (visible-frame-list))'
}

change_focus() {
    emacsclient -s $EMACS_SOCKET_NAME -n -e "(select-frame-set-input-focus (selected-frame))" > /dev/null
}

if [[ $(visible_frames) -lt  2 ]]; then
    # need to create a frame
    # -c $@ with no args just opens the scratch buffer
    exec emacsclient -s $EMACS_SOCKET_NAME -n -c "$@" && change_focus
else
    # there is already a visible frame besides the daemon, so
    # -n $@ errors if there are no args
    [[ $# -ne 0 ]] && emacsclient -s $EMACS_SOCKET_NAME -n "$@" && change_focus
fi

if [[ $? -ne 0 ]]; then
    echo "Failed to open file in emacs."
    exit 1
fi

# switch to emacs
osascript -e 'tell application "Emacs" to activate'
