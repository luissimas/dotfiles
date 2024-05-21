# Source path and env from .bash_profile
source ~/.bash_profile

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
alias killall-docker='docker ps -q | xargs -r docker stop'
alias k='kubectl'
alias hl='cd ~/projects/homelab'
alias dot='cd ~/dotfiles'
alias zet='cd $NOTES'
alias lg='lazygit'
alias proj='cd $(find ~/projects -maxdepth 1 -type d -print | fzf)'

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
