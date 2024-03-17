#!/usr/bin/env bash

# Use GNU Stow to symlink config files
for folder in $(ls -d */); do
    stow $folder
done
