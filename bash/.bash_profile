# Path
export PATH=$HOME/.local/go/bin:$PATH
export PATH=$HOME/dotfiles/scripts:$PATH
export PATH=$HOME/.config/emacs/bin:$PATH
export GOPATH=$HOME/.local/go

# Exports
export TERM="kitty"
export BROWSER="brave"
export READER="zathura"
export EDITOR="nvim"
export FZF_DEFAULT_COMMAND="fd --type file --hidden --follow --exclude .git --color=always"
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
export FZF_DEFAULT_OPTS="--border --ansi"
export FZF_ALT_C_OPTS="--preview 'tree -C {}'"
export LEDGER_FILE=~/Documents/accounting/accounting.journal

# Start WM
if [ -z "$DISPLAY" ] && [ "$XDG_VTNR" = 1 ]; then
  exec Hyprland
fi
