#!/bin/zsh

dotfilesdir=/home/padawan/dotfiles

cd $dotfilesdir

# Install packages from list
paru -S --needed -<packages.txt

# Use GNU Stow to symlink config files

for folder in $(ls -d */)
  stow $folder
