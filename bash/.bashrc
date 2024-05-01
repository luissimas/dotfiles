# Path
export PATH=$HOME/.local/go/bin:$PATH
export PATH=$HOME/dotfiles/scripts:$PATH
export PATH=$HOME/.config/emacs/bin:$PATH
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

# Aliases
alias v='nvim'
alias k='kubectl'
alias t='tmux'
alias ta='tmux attach'
alias ls='ls --color=auto'
alias ll='ls -lah'
alias ci='cd $(find . -type d -print | fzf)'
alias ef='fzf | xargs -r $EDITOR'
alias fd='fdfind'
alias killall-docker=docker ps -q | xargs -r docker stop

# If not running interactively, skip the rest
[[ $- != *i* ]] && return

# History
export HISTFILE=~/.histfile
export HISTSIZE=25000
export SAVEHIST=25000
export HISTCONTROL=ignorespace

# Prompt styling
# PS1='\[\e[1m\] \w $ \[\e[m\] '
PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '

# Enable FZF keybindings
source /usr/share/doc/fzf/examples/key-bindings.bash

# Enable eat integration
[ -n "$EAT_SHELL_INTEGRATION_DIR" ] && \
  source "$EAT_SHELL_INTEGRATION_DIR/bash"

# Hook direnv
eval "$(direnv hook bash)"
