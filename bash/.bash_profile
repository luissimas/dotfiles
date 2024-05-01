# Path
export PATH=$HOME/.local/go/bin:$PATH
export PATH=$HOME/dotfiles/scripts:$PATH
export PATH=$HOME/.config/emacs/bin:$PATH
export PATH=$HOME/.local/bin:$PATH
export GOPATH=$HOME/.local/go

# Exports
export TERM="xterm-256color"
export BROWSER="brave"
export EDITOR="nvim"
export FZF_DEFAULT_COMMAND="fdfind --type file --hidden --follow --exclude .git --color=always"
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
export FZF_DEFAULT_OPTS="--border --ansi"
export FZF_ALT_C_OPTS="--preview 'tree -C {}'"
export LEDGER_FILE=~/Documents/accounting/accounting.journal
