#
# ~/.bash_profile
#

# Exports
export EDITOR="nvim"
export TERMINAL="st"
export BROWSER="qutebrowser"
export READER="zathura"

[[ -f ~/.bashrc ]] && . ~/.bashrc

[[ $(fgconsole 2>/dev/null) == 1 ]] && exec startx -- vt1
