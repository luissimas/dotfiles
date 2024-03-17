#!/usr/bin/env bash

if [ ! $(eval "emacsclient -e 1 2> /dev/null") ]; then
    # emacs --daemon --no-x-resources
    emacs --daemon
fi

emacsclient -c -n
