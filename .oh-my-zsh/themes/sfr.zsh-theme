PROMPT="%{$fg[yellow]%}[%n@%m]"
PROMPT+=' %{$fg[blue]%}%c%{$reset_color%} $ '
RPROMPT=$'$(git_prompt_info)'
PS2="%{$fg[yellow]%}>>> %{$reset_color%}"

ZSH_THEME_GIT_PROMPT_PREFIX="["
ZSH_THEME_GIT_PROMPT_SUFFIX="]"
ZSH_THEME_GIT_PROMPT_DIRTY=""
ZSH_THEME_GIT_PROMPT_CLEAN=""



