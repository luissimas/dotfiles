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

    # Mise setup
    mise activate fish | source

    # Television setup
    tv init fish | source

    # Abbreviations
    abbr v nvim
    abbr t tmux
    abbr ta tmux attach
    abbr k kubectl
    abbr lg lazygit
    abbr open xdg-open
    abbr p project-switcher
end

function mkpass
    head -c 12 /dev/urandom | base64 -w 0
end
