# PATH
add_path() {
  [ -d "$1" ] && export PATH="$1:$PATH"
}

add_path "$HOME/.local/bin"
add_path "$HOME/scripts"
add_path "$HOME/dotfiles/scripts"
add_path "$HOME/.local/go/bin"
add_path "$HOME/work/scripts"
add_path "$HOME/work/scripts/factory"
add_path "$HOME/work/scripts/frontier"

export GOPATH=$HOME/.local/go

# Editor: prefer nvim, fall back to vim
if command -v nvim >/dev/null 2>&1; then
  export EDITOR=nvim
else
  export EDITOR=vim
fi

# Homebrew (macOS)
if [ -x /opt/homebrew/bin/brew ]; then
  eval "$(/opt/homebrew/bin/brew shellenv)"
fi

# Homebrew (linuxbrew)
if [ -x /home/linuxbrew/.linuxbrew/bin/brew ]; then
  eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"
fi

# Cargo
if [ -f "$HOME/.cargo/env" ]; then
  . "$HOME/.cargo/env"
fi

# Nix
if [ -e "$HOME/.nix-profile/etc/profile.d/nix.sh" ]; then
  . "$HOME/.nix-profile/etc/profile.d/nix.sh"
fi

# Git identity on CERN servers
if [ -d /afs ]; then
  export GIT_AUTHOR_NAME="Luís Simas"
  export GIT_AUTHOR_EMAIL="luis.simas@cern.ch"
fi

# EOS home on CERN servers
[ -d /afs ] && export EOSHOME="/eos/home-l/lsimasde"

# Source interactive setup for login shells
[ -f ~/.bashrc ] && . ~/.bashrc
