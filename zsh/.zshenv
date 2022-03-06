# Path
export PATH="${PATH}:${HOME}/.local/bin/:${HOME}/bin:${HOME}/.emacs.d/bin:${HOME}/repos/elixir-ls:${HOME}/.cargo/bin"
export PATH=$HOME/.local/go/bin:$PATH
export PATH=$HOME/.config/bspwm/scripts:$PATH
export PATH=$HOME/.config/polybar/scripts:$PATH
export PATH=$HOME/scripts:$PATH
export GOPATH=$HOME/.local/go
export QUTEWAL_DYNAMIC_LOADING=True
export LEDGER_FILE=$HOME/dox/accounting.journal

# Exports
export BROWSER="qutebrowser"
export READER="zathura"
export EDITOR="nvim"
export FZF_DEFAULT_COMMAND="fd --type f --hidden --follow --exclude .git"
export FZF_DEFAULT_OPTS="--color=16 --no-mouse --height 40%" # --preview 'bat {} --color=always -p'"

# Skipping compinit
skip_global_compinit=1
