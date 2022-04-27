#!/bin/sh

wallpaperDirectory=/home/padawan/.wal/

# Prompt if the script should use the wallpaper or the themes
option=$(echo -e 'Theme\nWallpaper\nPywal' | dmenu -i -p "Source")

case $option in "Pywal")
	selected=$(ls $wallpaperDirectory | dmenu -i -p "Select a file")

	if [[ -z "$selected" ]]; then
		printf "No image selected\n"
	else
		themelight=$(echo -e 'Dark\nLight' | dmenu -i -p "Select")

		if [[ -z "$themelight" ]]; then
			printf "No selection\n"
		else
			# Update pywal theme
			printf "Setting pywal theme...\n"

			case $themelight in "Dark")
				wal -i "$wallpaperDirectory$selected" -q
				;;
			"Light")
				wal -l -i "$wallpaperDirectory$selected" -q
				;;
			esac

			# Update bspwm colors
			bspwmcolors

			# Update qutebrowser colors
			pgrep qutebrowser >/dev/null && qutebrowser :config-source

			echo "wally" >~/.colorscheme

			# Update betterlockscreen (takes a while)
			printf "Setting lock screen...\n"
			betterlockscreen -u "$wallpaperDirectory$selected" >/dev/null 2>&1
			printf "Done.\n"
		fi
	fi
	;;
"Theme")
	selected=$(echo -e 'Nord\nCatppuccin\nPalenight\nGruvbox\nTokyonight\nRosé Pine\nModus Vivendi\nModus Operandi' | dmenu -i -p "Theme")

	if [[ -z $selected ]]; then
		printf "No selection"
	else
		case $selected in "Palenight")
			name=base16-material-palenight
			theme=~/.config/wal/colorschemes/palenight
			bg=~/.wal/palenight.jpg
			;;
		"Gruvbox")
			name=base16-gruvbox-dark-hard
			theme="base16-gruvbox-hard"
			bg=~/.wal/forestfog.jpg
			;;
		"Rosé Pine")
			name=rose-pine
			theme=~/.config/wal/colorschemes/rosepine.json
			bg=~/.wal/rosessky.jpg
			;;
		"Tokyonight")
			name=tokyonight
			theme=~/.config/wal/colorschemes/tokyonight.json
			bg=~/.wal/traintunnel.jpg
			;;
		"Modus Vivendi")
			name=modus-vivendi
			theme=~/.config/wal/colorschemes/modus-vivendi.json
			bg=~/.wal/whitemountain.jpg
			;;
		"Modus Operandi")
			name=modus-operandi
			theme=~/.config/wal/colorschemes/modus-operandi.json
			bg=~/.wal/whitemountain.jpg
			;;
		"Nord")
			name=nord
			theme=~/.config/wal/colorschemes/nord.json
			bg=~/.wal/nordmountain.jpeg
			;;
		"Catppuccin")
			name=catppuccin
			theme=~/.config/wal/colorschemes/catppuccin.json
			bg=~/.wal/catppuccin.jpg
			;;
		*) printf "No option selected." ;;
		esac

		wal --theme $theme

		feh --bg-fill $bg

		echo $name >~/.colorscheme

		# Update bspwm colors
		bspwmcolors

		# Update emacs colors
		emacsclient -e "(pada/load-system-theme)"

		# Recompiling dmenu
		sudo recompile_dmenu

		# Update qutebrowser colors
		pgrep qutebrowser >/dev/null && qutebrowser :config-source
	fi
	;;
"Wallpaper")
	selected=$(ls $wallpaperDirectory | dmenu -i -p "File")

	if [[ -z "$selected" ]]; then
		printf "No image selected\n"
	else
		#Set wallpaper with feh
		printf "Setting wallpaper...\n"
		feh --bg-fill $wallpaperDirectory$selected
		betterlockscreen -u "$wallpaperDirectory$selected" >/dev/null 2>&1
		printf "Done.\n\n"
	fi
	;;
*)
	printf "No option selected\n"
	;;
esac
