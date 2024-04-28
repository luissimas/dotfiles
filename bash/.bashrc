# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# History
export HISTFILE=~/.histfile
export HISTSIZE=25000
export SAVEHIST=25000
export HISTCONTROL=ignorespace

# Prompt styling
PS1='\[\e[1m\] \w $ \[\e[m\] '

# Aliases
alias v='nvim'
alias k='kubectl'
alias t='tmux'
alias ta='tmux attach'
alias ls='ls --color=auto'
alias ll='ls -lah'
alias ci='cd $(find . -type d -print | fzf)'
alias ef='fzf | xargs -r $EDITOR'

# Enable FZF keybindings
if command -v fzf-share >/dev/null; then
  source "$(fzf-share)/key-bindings.bash"
  source "$(fzf-share)/completion.bash"
fi

# Enable eat integration
[ -n "$EAT_SHELL_INTEGRATION_DIR" ] && \
  source "$EAT_SHELL_INTEGRATION_DIR/bash"

# Hook direnv
eval "$(direnv hook bash)"
