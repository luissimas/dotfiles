#!/bin/bash

if [ ! $(eval "emacsclient -e 1 2> /dev/null") ]; then
    emacs --daemon --no-x-resources
    # emacs --daemon --with-profile doom
fi

emacsclient -c -n
