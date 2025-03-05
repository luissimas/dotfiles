# Set PATH
fish_add_path ~/.local/bin
fish_add_path ~/.local/go/bin
fish_add_path ~/dotfiles/scripts
fish_add_path ~/.krew/bin

# Set env variables
set -Ux KUBECTX_IGNORE_FZF 1

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
    source "$HOME/.cargo/env.fish"

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
end

function mkpass -d "Create a random password and copy it to the clipboard"
    head -c 12 /dev/urandom | base64 -w 0 | wl-copy
end
