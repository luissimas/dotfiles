# Path
export PATH=$HOME/.local/go/bin:$PATH
export PATH=$HOME/scripts:$PATH
export GOPATH=$HOME/.local/go

# Exports
export TERM="kitty"
export BROWSER="brave"
export READER="zathura"
export EDITOR="emacsclient -c"
export FZF_DEFAULT_COMMAND="fd --type file --hidden --follow --exclude .git --color=always"
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
export FZF_DEFAULT_OPTS="--border --ansi --preview 'bat --style=numbers --color=always {}' --bind 'ctrl-/:toggle-preview'"
export FZF_ALT_C_OPTS="--preview 'tree -C {}'"
