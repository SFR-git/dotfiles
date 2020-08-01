ZSH_THEME="sfr"

CASE_SENSITIVE="false"
HYPHEN_INSENSITIVE="true"

plugins=(git zsh-autosuggestions colored-man-pages command-not-found cp compleat)

# Exports
export LANG=en_GB.UTF-8
export HISTCONTROL=ignoreboth
export EDITOR=nvim
export VISUAL=nvim
export ZSH="/home/sol/.oh-my-zsh"
export PATH=$HOME/bin:/usr/local/bin:$PATH
export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=#545454"

source $ZSH/oh-my-zsh.sh

# Aliases
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
alias dotfiles='/usr/bin/git --git-dir=/home/sol/.dots/ --work-tree=/home/sol'
alias scrot='scrot ~/Pictures/Screenshots/%b%d::%H%M%S.png'

# Rick roll roulette
[ $[$RANDOM % 420] = 0 ] && ./.rickrollrc.sh