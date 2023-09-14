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
alias v='vim'
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
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh

# Accept autosuggestion
bindkey '^ ' autosuggest-accept

# Enable prompt
autoload -U promptinit
promptinit

# Prompt Style
PROMPT='%B%~%F{yellow}${vcs_info_msg_0_}%f%b λ '

# opam configuration
test -r /home/padawan/.opam/opam-init/init.zsh && . /home/padawan/.opam/opam-init/init.zsh >/dev/null 2>/dev/null || true

# If not in tmux or emacs, attach to session
# if [ -f $TMUX ] && [ -z $INSIDE_EMACS ]; then tmux attach 2>/dev/null; fi

export PATH="$HOME/.yarn/bin:$HOME/.config/yarn/global/node_modules/.bin:$PATH"

# Completion for asdf
. /opt/asdf-vm/asdf.sh

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/home/padawan/Downloads/google-cloud-sdk/path.zsh.inc' ]; then . '/home/padawan/Downloads/google-cloud-sdk/path.zsh.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/home/padawan/Downloads/google-cloud-sdk/completion.zsh.inc' ]; then . '/home/padawan/Downloads/google-cloud-sdk/completion.zsh.inc'; fi

autoload -U +X bashcompinit && bashcompinit
complete -o nospace -C /usr/bin/terraform terraform
