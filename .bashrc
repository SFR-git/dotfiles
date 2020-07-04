#   _____ ______ _____
#  / ____|  ____|  __ \  || Programming enthausiast, privacy advocate, free software user.
# | (___ | |__  | |__) | ||
#  \___ \|  __| |  _  /  || github.com/SFR-git
#  ____) | |    | | \ \  || ko-fi.com/supportsfr
# |_____/|_|    |_|  \_\ || /u/sfrvtma

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Make colorcoding available for everyone

Black='\[\e[0;30m\]'	# Black
Red='\[\e[0;31m\]'		# Red
Green='\[\e[0;32m\]'	# Green
Yellow='\[\e[0;33m\]'	# Yellow
Blue='\[\e[0;34m\]'		# Blue
Purple='\[\e[0;35m\]'	# Purple
Cyan='\[\e[0;36m\]'		# Cyan
White='\[\e[0;37m\]'	# White

# Bold
BBlack='\[\e[1;30m\]'	# Black
BRed='\[\e[1;31m\]'		# Red
BGreen='\[\e[1;32m\]'	# Green
BYellow='\[\e[1;33m\]'	# Yellow
BBlue='\[\e[1;34m\]'	# Blue
BPurple='\[\e[1;35m\]'	# Purple
BCyan='\[\e[1;36m\]'	# Cyan
BWhite='\[\e[1;37m\]'	# White

# Background
On_Black='\[\e[40m\]'	# Black
On_Red='\[\e[41m\]'		# Red
On_Green='\[\e[42m\]'	# Green
On_Yellow='\[\e[43m\]'	# Yellow
On_Blue='\[\e[44m\]'	# Blue
On_Purple='\[\e[45m\]'	# Purple
On_Cyan='\[\e[46m\]'	# Cyan
On_White='\[\e[47m\]'	# White

NC='\[\e[m\]'			# Color Reset

ALERT="${BWhite}${On_Red}" # Bold White on red background

# Useful aliases
alias c='clear'
alias ..='cd ..'
alias ls='exa -a --git --long -G -s name --color=auto'
alias mkdir='mkdir -pv'
alias free='free -mt'
alias ps='ps auxf'
alias psgrep='ps aux | grep -v grep | grep -i -e VSZ -e'
alias wget='wget -c'
alias histg='history | grep'
alias myip='curl ipv4.icanhazip.com'
alias grep='grep --color=auto'
alias mv='mv -i'
alias rm='rm -i'
alias cp='cp -i'
alias vim='nvim'
alias vifm='~/.config/vifm/scripts/vifmrun'
alias config='/usr/bin/git --git-dir=/home/sol/.dots/ --work-tree=/home/sol'
alias scrot='scrot ~/Pictures/Screenshots/%b%d::%H%M%S.png'
alias sduo='curl -s -L https://raw.githubusercontent.com/keroserene/rickrollrc/master/roll.sh | bash'

# Exports
export HISTCONTROL=ignoreboth
export EDITOR=nvim
export VISUAL=nvim

# Set PATH so it includes user's private bin directories
PATH="${HOME}/bin:${HOME}/.local/bin:${PATH}"

# Set prompt
PS1="${Yellow}[\u@\h]${NC} ${Blue}\w${NC} \\$ "
PS2="${Yellow}>>>${NC} "
