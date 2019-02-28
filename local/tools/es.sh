#!/bin/bash

# simple script to shutdown the running Emacs daemon

# emacsclient options for reference
# -a Alternate editor, runs bin/false in this case
# -e eval the script

# If the server-process is bound and the server is in a good state, then kill
# the server

if [[ ! -e $EMACS_SOCKET_NAME ]]; then
    echo 'Please Config $EMACS_SOCKET_NAME First!'
    exit 1
fi

server_ok() {
    emacsclient -s $EMACS_SOCKET_NAME -a "false" -e "(boundp 'server-process)"
}

if [ "t" == "$(server_ok)" ]; then
  echo "Shutting down Emacs server"
  # wasn't removing emacs from ALT-TAB on mac
  # emacsclient -e "(server-force-delete)"
  emacsclient  -s $EMACS_SOCKET_NAME -e '(kill-emacs)'
else
  echo "Emacs server not running"
fi
