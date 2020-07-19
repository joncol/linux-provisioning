#!/usr/bin/env bash

# Use this as display-setup-script in `lightdm.conf`.
setxkbmap -model pc105 -layout us,us -variant colemak,altgr-intl -option -option "grp:shifts_toggle,ctrl:nocaps,terminate:ctrl_alt_bksp"
