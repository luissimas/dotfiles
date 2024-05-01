#!/usr/bin/env bash

echo "Symlinking directories with stow..."

# Use GNU Stow to symlink config files
for folder in $(ls -d */ | sed "s/\/$//"); do
    echo -n -e "\t linking $folder... "
    stow $folder
echo "Done."
done

echo "Directories linked sucessfully!"
