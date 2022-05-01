# Lines configured by zsh-newuser-install

# History
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000

# Zsh opts
setopt autocd extendedglob nomatch PROMPT_SUBST

# Disable beep just in case
unsetopt beep

# Enable autocomplete
autoload -U compinit
zstyle ':completion:*' select
compinit

# Completion for kitty
kitty + complete setup zsh | source /dev/stdin

# Include hidden files on completion
_comp_options+=(globdots)

# Disable completion aliases
unsetopt complete_aliases

# Enable colors
autoload -U colors && colors

# Vi mode
bindkey -v
export KEYTIMEOUT=1

# Change cursor shape for different vi modes.
function zle-keymap-select {
	if [[ ${KEYMAP} == vicmd ]] ||
		[[ $1 = 'block' ]]; then
		echo -ne '\e[1 q'
	elif [[ ${KEYMAP} == main ]] ||
		[[ ${KEYMAP} == viins ]] ||
		[[ ${KEYMAP} = '' ]] ||
		[[ $1 = 'beam' ]]; then
		echo -ne '\e[5 q'
	fi
}

zle -N zle-keymap-select
zle-line-init() {
	zle -K viins # initiate `vi insert` as keymap (can be removed if `bindkey -V` has been set elsewhere)
	echo -ne "\e[5 q"
}
zle -N zle-line-init
echo -ne '\e[5 q'                # Use beam shape cursor on startup.
preexec() { echo -ne '\e[5 q'; } # Use beam shape cursor for each new prompt.

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

# Habitica cli tool
alias hbt='python ~/fun/python/habitica-cli-py/main.py'

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

# Syntax highlighting and autosuggestions
if [ $(find /sys/class/power_supply -name "*BAT*") ]; then
	source /usr/share/zsh-syntax-highlighting
	source /usr/share/zsh-autosuggestions
else
	source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
	source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh
fi

# Accept autosuggestion
bindkey '^ ' autosuggest-accept

# Enable prompt
autoload -U promptinit
promptinit

# Prompt Style (currently disabled because I'm using spaceship prompt)
# PROMPT='%B%~%F{yellow}${vcs_info_msg_0_}%f%b λ '

# Spaceship prompt
SPACESHIP_PROMPT_ADD_NEWLINE=false
SPACESHIP_PROMPT_SEPARATE_LINE=false
SPACESHIP_CHAR_SUFFIX=" "
SPACESHIP_CHAR_SYMBOL=λ
SPACESHIP_USER_SHOW=true
SPACESHIP_GIT_SHOW=true
SPACESHIP_GIT_BRANCH_SHOW=true
SPACESHIP_GIT_STATUS_SHOW=false
SPACESHIP_DIR_TRUNC=2
SPACESHIP_DIR_TRUNC_REPO=true
SPACESHIP_DIR_LOCK_SYMBOL="  "
SPACESHIP_EXEC_TIME_SHOW=true
SPACESHIP_EXEC_TIME_ELAPSED=2
SPACESHIP_VI_MODE_SHOW=false
SPACESHIP_BATTERY_SHOW=false

# Removing all package manager prompt stuff
SPACESHIP_PACKAGE_SHOW=false
SPACESHIP_NODE_SHOW=true
SPACESHIP_RUBY_SHOW=false
SPACESHIP_ELM_SHOW=false
SPACESHIP_ELIXIR_SHOW=true
SPACESHIP_XCODE_SHOW_LOCAL=false
SPACESHIP_SWIFT_SHOW_LOCAL=false
SPACESHIP_GOLANG_SHOW=false
SPACESHIP_PHP_SHOW=false
SPACESHIP_RUST_SHOW=false
SPACESHIP_HASKELL_SHOW=false
SPACESHIP_JULIA_SHOW=false
SPACESHIP_DOCKER_SHOW=false
SPACESHIP_DOCKER_CONTEXT_SHOW=false
SPACESHIP_AWS_SHOW=false
SPACESHIP_GCLOUD_SHOW=false
SPACESHIP_VENV_SHOW=false
SPACESHIP_CONDA_SHOW=false
SPACESHIP_PYENV_SHOW=false
SPACESHIP_DOTNET_SHOW=false
SPACESHIP_EMBER_SHOW=false
SPACESHIP_KUBECTL_SHOW=false
SPACESHIP_KUBECTL_VERSION_SHOW=false
SPACESHIP_KUBECONTEXT_SHOW=false
SPACESHIP_GRADLE_SHOW=false
SPACESHIP_MAVEN_SHOW=false
SPACESHIP_TERRAFORM_SHOW=false

# Setting spaceship as the prompt
prompt spaceship

# opam configuration
test -r /home/padawan/.opam/opam-init/init.zsh && . /home/padawan/.opam/opam-init/init.zsh >/dev/null 2>/dev/null || true

# If not in tmux or emacs, attach to session
if [ -f $TMUX ] && [ -z $INSIDE_EMACS ]; then tmux attach 2>/dev/null; fi
fpath=($fpath "/home/liven/.zfunctions")
fpath=($fpath "/home/liven/.zfunctions")
