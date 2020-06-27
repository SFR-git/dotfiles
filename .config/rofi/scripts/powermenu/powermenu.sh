#!/usr/bin/env bash

rofi_command="rofi -theme scripts/powermenu/powermenu.rasi"
USERNAME=`whoami`

# Options
shutdown="Shutdown"
reboot="Restart"
suspend="Suspend"
logout="Logout"

# Variable passed to rofi
options="$shutdown\n$reboot\n$suspend\n$logout"

chosen="$(echo -e "$options" | $rofi_command -dmenu )"
case $chosen in
    $shutdown)
        systemctl poweroff
        ;;
    $reboot)
        systemctl reboot
        ;;
    $suspend)
        systemctl suspend
        ;;
    $logout)
        pkill -u $USERNAME
        ;;
esac
