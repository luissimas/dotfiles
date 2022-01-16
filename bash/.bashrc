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

export PATH="${PATH}:${HOME}/.local/bin/:${HOME}/.scripts/"

# Custom cd function
c() {
	cd $1 && exa -la --no-user --time-style long-iso --icons
}

# Vterm
vterm_printf() {
	if [ -n "$TMUX" ] && ([ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ]); then
		# Tell tmux to pass the escape sequences through
		printf "\ePtmux;\e\e]%s\007\e\\" "$1"
	elif [ "${TERM%%-*}" = "screen" ]; then
		# GNU screen (screen, screen-256color, screen-256color-bce)
		printf "\eP\e]%s\007\e\\" "$1"
	else
		printf "\e]%s\e\\" "$1"
	fi
}

# Aliases
alias l='exa -l --no-user --time-style long-iso --icons'
alias la='exa -la --no-user --time-style long-iso --icons'
alias tree='exa -T --icons'
alias grep='rg'
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
alias v='nvim'
alias vim='nvim'
alias code='codium'
alias icat='kitty +kitten icat'
alias cat='bat -P'
alias mux='tmuxp'
alias ta='tmux attach'
alias recompile='sudo ~/scripts/recompile-suckless.sh'

# opam configuration
test -r /home/padawan/.opam/opam-init/init.sh && . /home/padawan/.opam/opam-init/init.sh >/dev/null 2>/dev/null || true


# BEGIN_KITTY_SHELL_INTEGRATION
if test -n "$KITTY_INSTALLATION_DIR" -a -e "$KITTY_INSTALLATION_DIR/shell-integration/bash/kitty.bash"; then source "$KITTY_INSTALLATION_DIR/shell-integration/bash/kitty.bash"; fi
# END_KITTY_SHELL_INTEGRATION
