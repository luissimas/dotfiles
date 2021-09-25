#!/bin/sh

if [ ! $(eval "emacsclient -e 1 2> /dev/null") ]; then
    emacs --daemon --no-x-resources
fi

emacsclient -c -n
