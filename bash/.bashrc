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
alias killall-docker='docker ps -q | xargs -r docker stop'
alias k='kubectl'
alias hl='cd ~/projects/homelab'
alias dot='cd ~/dotfiles'
alias zet='cd $NOTES'
alias lg='lazygit'
alias proj='project-switcher'
alias open='xdg-open'

# If not running interactively, skip the rest
[[ $- != *i* ]] && return

# History
export HISTFILE=~/.histfile
export HISTSIZE=25000
export SAVEHIST=25000
export HISTCONTROL=ignorespace

# Mise setup
# eval "$(~/.local/bin/mise activate bash)"

# Starship setup
eval "$(starship init bash)"

# Television setup
eval "$(tv init bash)"

# Incus development. I kinda miss NixOS + direnv, ngl
export CGO_CFLAGS="-I/home/padawan/.local/go/deps/raft/include/ -I/home/padawan/.local/go/deps/cowsql/include/"
export CGO_LDFLAGS="-L/home/padawan/.local/go/deps/raft/.libs -L/home/padawan/.local/go/deps/cowsql/.libs/"
export LD_LIBRARY_PATH="/home/padawan/.local/go/deps/raft/.libs/:/home/padawan/.local/go/deps/cowsql/.libs/"
export CGO_LDFLAGS_ALLOW="(-Wl,-wrap,pthread_create)|(-Wl,-z,now)"
. "$HOME/.cargo/env"
