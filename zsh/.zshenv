# Path
export PATH="${PATH}:${HOME}/.local/bin/:${HOME}/bin:${HOME}/.emacs.d/bin:${HOME}/repos/elixir-ls:${HOME}/.cargo/bin"
export PATH=$HOME/.local/go/bin:$PATH
export PATH=$HOME/.config/bspwm/scripts:$PATH
export PATH=$HOME/.config/polybar/scripts:$PATH
export PATH=$HOME/scripts:$PATH
export PATH=$HOME/.npm-packages/bin:$PATH
export PATH=$HOME/repos/doom/bin:$PATH
export PATH=$HOME/repos/lexical/_build/dev/rel/lexical:$PATH
export FLYCTL_INSTALL="${HOME}/.fly"
export PATH="$FLYCTL_INSTALL/bin:$PATH"
export GOPATH=$HOME/.local/go
export QUTEWAL_DYNAMIC_LOADING=True
export LEDGER_FILE=$HOME/docs/Accounting/accounting.journal
export PYTHON_KEYRING_BACKEND=keyring.backends.null.Keyring
export KERL_CONFIGURE_OPTIONS="--without-wx --without-javac"

# Exports
export BROWSER="brave"
export READER="zathura"
export EDITOR="nvim"
export FZF_DEFAULT_COMMAND="fd --type f --hidden --follow --exclude .git"
export FZF_DEFAULT_OPTS="--color=16 --no-mouse --height 40%" # --preview 'bat {} --color=always -p'"
export OPENWEATHER_KEY="3b6c414b301c5501f7cfe3b433d89d7f"

# Skipping compinit
skip_global_compinit=1
