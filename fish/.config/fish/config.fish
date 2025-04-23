# Set PATH
fish_add_path ~/.local/bin
fish_add_path ~/.local/go/bin
fish_add_path ~/dotfiles/scripts
fish_add_path ~/.krew/bin

# Set env variables
set -Ux KUBECTX_IGNORE_FZF 1

# If we're in an interactive shell
if status is-interactive
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
    abbr i incus
end

function mkpass -d "Create a random password and copy it to the clipboard"
    head -c 12 /dev/urandom | base64 -w 0 | wl-copy
end

function fish_greeting
    if ! test -e /tmp/pada-motd
        echo "Message of the day"
        echo "Pending nodes in your Inbox"
        echo "==========================="
        find ~/projects/zettelkasten/Inbox/* -maxdepth 1 -type f -name "*.md" -exec basename {} .md \;
        touch /tmp/pada-motd
    end
end
