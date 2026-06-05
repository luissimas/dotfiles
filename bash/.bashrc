# If not running interactively, skip the rest
[[ $- != *i* ]] && return

# History
export HISTSIZE=100000
export HISTFILESIZE=100000
export HISTCONTROL=ignoreboth:erasedups
shopt -s histappend
PROMPT_COMMAND='history -a'

# Shell options
shopt -s autocd cdspell dirspell globstar checkwinsize

# Vi mode
set -o vi
# Alt-. (last-arg) isn't bound in vi insert mode by default
bind -m vi-insert '"\e.": yank-last-arg'

# Free Ctrl-S for forward incremental search
stty -ixon 2>/dev/null

# Bash completion
if [ -f /usr/share/bash-completion/bash_completion ]; then
  . /usr/share/bash-completion/bash_completion
elif [ -f /etc/bash_completion ]; then
  . /etc/bash_completion
elif [ -f /opt/homebrew/etc/profile.d/bash_completion.sh ]; then
  . /opt/homebrew/etc/profile.d/bash_completion.sh
fi

# git-prompt (__git_ps1)
for f in \
  /usr/share/git-core/contrib/completion/git-prompt.sh \
  /usr/share/git/git-prompt.sh \
  /usr/lib/git-core/git-sh-prompt \
  /opt/homebrew/etc/bash_completion.d/git-prompt.sh; do
  if [ -f "$f" ]; then
    . "$f"
    break
  fi
done
GIT_PS1_SHOWDIRTYSTATE=1
GIT_PS1_SHOWUNTRACKEDFILES=1

# Prompt
__prompt() {
  local exit=$?
  local reset='\[\e[0m\]'
  local dim='\[\e[2m\]'
  local red='\[\e[31m\]'
  local green='\[\e[32m\]'
  local yellow='\[\e[33m\]'
  local cyan='\[\e[36m\]'
  local host_color
  if [ -n "$SSH_CONNECTION" ]; then
    host_color="$yellow"
  else
    host_color="$green"
  fi
  local branch=""
  if command -v __git_ps1 >/dev/null 2>&1; then
    branch="$dim$(__git_ps1 ' (%s)')$reset"
  fi
  local marker
  if [ $exit -eq 0 ]; then
    marker="${reset}\$ "
  else
    marker="${red}\$ ${reset}"
  fi
  PS1="${host_color}\u@\h${reset}:${cyan}\w${reset}${branch} ${marker}"
}
PROMPT_COMMAND='history -a; __prompt'
