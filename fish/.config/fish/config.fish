# Set PATH
fish_add_path ~/.local/bin
fish_add_path ~/.local/go/bin
fish_add_path ~/dotfiles/scripts
fish_add_path ~/.krew/bin

if test -e /opt/homebrew/bin/
    fish_add_path /opt/homebrew/bin/
end

if test -e /home/linuxbrew/.linuxbrew/bin
    fish_add_path /home/linuxbrew/.linuxbrew/bin
end

# Setup homebrew
eval "$(brew shellenv)"

# Set env variables
set -Ux KUBECTX_IGNORE_FZF 1
set -Ux EDITOR nvim

# If we're in an interactive shell
if status is-interactive
    # Disable greeting message
    set fish_greeting

    # vi bindings
    fish_vi_key_bindings
    set -g fish_vi_force_cursor 1
    set fish_cursor_default block
    set fish_cursor_insert line
    set fish_cursor_replace_one underscore

    # Television setup
    tv init fish | source

    # Since we're using vi mode, we need to explicitly bind in insert mode
    bind --mode insert \cr tv_shell_history
    bind --mode insert \ct tv_smart_autocomplete

    # Starship setup
    starship init fish | source

    # Direnv setup
    direnv hook fish | source

    # Cargo setup
    if test -e "$HOME/.cargo/env.fish"
        source "$HOME/.cargo/env.fish"
    end

    # Abbreviations
    abbr v nvim
    abbr t tmux
    abbr ta tmux attach
    abbr k kubectl
    abbr lg lazygit
    abbr open xdg-open
    abbr p project-switcher
    abbr nix-shell nix-shell --run fish
    abbr j juju
    abbr i incus
end

function mkpass -d "Create a random password and copy it to the clipboard"
    head -c 12 /dev/urandom | base64 -w 0 | wl-copy
end
