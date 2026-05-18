#!/bin/sh
input=$(cat)

cwd=$(echo "$input" | jq -r '.workspace.current_dir // .cwd')
model=$(echo "$input" | jq -r '.model.display_name')
used=$(echo "$input" | jq -r '.context_window.used_percentage // empty')

# Shorten home directory to ~
home="$HOME"
short_cwd="${cwd/#$home/~}"

# Git branch (skip optional locks)
branch=$(git -C "$cwd" branch --show-current 2>/dev/null)

# Build the status line
if [ -n "$branch" ]; then
    dir_part="$short_cwd ($branch)"
else
    dir_part="$short_cwd"
fi

if [ -n "$used" ]; then
    ctx_part=" | ctx: $(printf '%.0f' "$used")%"
else
    ctx_part=""
fi

printf '\033[34m%s\033[0m | \033[35m%s\033[0m%s' "$dir_part" "$model" "$ctx_part"
