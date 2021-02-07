#!/bin/zsh

dotfilesdir=/home/padawan/dotfiles

cd $dotfilesdir

# Install packages from list
paru -S --needed -<packages.txt

# Use GNU Stow to symlink config files
stow alacritty
stow bash
stow betterlockscreen
stow bspwm
stow lf
stow nvim
stow polybar
stow pywal
stow qutebrowser
stow rofi
stow scripts
stow sxhkd
stow wallpapers
stow xinit
stow zathura
stow zsh
