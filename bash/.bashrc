#
# ~/.bashrc
#


# Exports
export TERM="st"
export BROWSER="qutebrowser"
export READER="zathura"
export EDITOR="nvim"

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Prompt Style
PS1='\e[1m \w λ \e[m'

#pywal
export PATH="${PATH}:${HOME}/.local/bin/:${HOME}/.scripts/"

# Functions

print(){
	if [ -z "$1" ] 
	then
		scrot -s -f ~/img/Screenshots/%Y_%m_%d_at_%H:%M:%S.png
		echo "Screenshot saved at: ~/img/Screenshots"
	else
		scrot -s -f "$1"
		echo "Screenshot saved at: $1"
	fi
}

# Aliases
alias l='ls --color=auto'
alias la='ls --color=auto -A'

alias rm='rm -i'
alias rd='rm -rf -i'

alias ..='cd ..'
alias ...='cd ../..'

alias addall='git add -A'
alias commit='git commit'
alias pull='git pull origin'
alias push='git push origin'
alias status='git status'
alias diff='git diff'
alias log='git log'
alias clone='git clone'

alias pacman='sudo pacman'

alias vim='nvim'

alias recompile='sudo recompile-suckless.sh'

# Alias for dotfiles git bare repo
alias config='/usr/bin/git --git-dir=$HOME/dotfiles/ --work-tree=$HOME'

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
