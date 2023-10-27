#!/usr/bin/env bash

## Author : Aditya Shakya (adi1090x)
## Github : @adi1090x
#
## Rofi   : Power Menu


uptime="$(uptime | tr "," " " | cut -f6-8 -d" ")"

# Options
shutdown=''
reboot=''
lock=''
suspend=''
logout=''
yes=''
no=''

# Rofi CMD
rofi_cmd() {
    rofi -dmenu \
        -p "Goodbye" \
        -mesg "Uptime: $(echo $uptime | awk -F':' '{print $1}') hours and $(echo $uptime | awk -F':' '{print $2}') minutes" \
        -theme "$CONFIG_HOME/rofi-scripts/powermenu/style.rasi"
}

# Confirmation CMD
confirm_cmd() {
    rofi -dmenu \
        -p "Are you sure you want to $1?" \
        -theme "$CONFIG_HOME/rofi-scripts/powermenu/confirm.rasi"
}

# Ask for confirmation
confirm_exit() {
    echo -e "$yes\n$no" | confirm_cmd $1
}

# Pass variables to rofi dmenu
run_rofi() {
    echo -e "$lock\n$suspend\n$logout\n$reboot\n$shutdown" | rofi_cmd
}

# Execute Command
run_cmd() {
    selected="$(confirm_exit ${1:2})"
    if [[ "$selected" == "$yes" ]]; then
        if [[ $1 == '--shutdown' ]]; then
            systemctl poweroff
        elif [[ $1 == '--reboot' ]]; then
            systemctl reboot
        elif [[ $1 == '--suspend' ]]; then
            systemctl suspend
        elif [[ $1 == '--logout' ]]; then
            kitty sh -c "$DESKTOP_SESSION"
            if [[ "$(basename $DESKTOP_SESSION)" == 'none+xmonad' ]]; then
                for i in $(pidof "xmonad-x86_64-linux"); do kill -kill $i; done
            elif [[ "$(basename $DESKTOP_SESSION)" == 'hyprland' ]]; then
                kill "$(pidof "Hyprland")"
            fi
            echo -n "logout"
        fi
    else
        exit 0
    fi
}

# Actions
chosen="$(run_rofi)"
case ${chosen} in
    $shutdown)
        run_cmd --shutdown
        ;;
    $reboot)
        run_cmd --reboot
        ;;
    $lock)
        if [[ -x '/usr/bin/betterlockscreen' ]]; then
            betterlockscreen -l
        elif [[ -x '/usr/bin/i3lock' ]]; then
            i3lock
        fi
        ;;
    $suspend)
        run_cmd --suspend
        ;;
    $logout)
        run_cmd --logout
        ;;
esac
