#!/usr/bin/env bash

rofi_command="rofi -theme scripts/powermenu/powermenu.rasi"

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
        mpc -q pause
        amixer set Master mute
        systemctl suspend
        ;;
    $logout)
        logout
        ;;
esac
