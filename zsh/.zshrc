# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
setopt autocd extendedglob nomatch
unsetopt beep
bindkey -v
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/padawan/.zshrc'

autoload -Uz compinit
compinit
_comp_options+=(globdots)
# End of lines added by compinstall



# User config
#

unsetopt complete_aliases
autoload -U colors && colors

# Prompt Style
PS1='%~ λ '

# Aliases
alias l='ls --color=auto'
alias la='ls --color=auto -A'

alias grep='grep --color=auto'

alias ..='cd ..'
alias ...='cd ../..'
alias c='customcd'

customcd () {
  cd $1
  ls --color=auto -A 
}

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

alias recompile='sudo ~/scripts/recompile-suckless.sh'

# Habitica cli tool 
alias hbt='python ~/fun/python/habitica-cli-py/main.py'

# Vi mode configs
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
echo -ne '\e[5 q' # Use beam shape cursor on startup.
preexec() { echo -ne '\e[5 q' ;} # Use beam shape cursor for each new prompt.


# syntax highlighting
source /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh 2>/dev/null
