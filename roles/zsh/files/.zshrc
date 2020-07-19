# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
    source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

if [[ -f "$HOME/.config/zsh/functions" ]]; then
    source "$HOME/.config/zsh/functions"
fi

zstyle ':completion:*' menu select
zmodload zsh/complist

# use the vi navigation keys in menu completion
bindkey -M menuselect 'h' vi-backward-char
bindkey -M menuselect 'k' vi-up-line-or-history
bindkey -M menuselect 'l' vi-forward-char
bindkey -M menuselect 'j' vi-down-line-or-history

alias git=hub
alias a="gcalcli agenda now tomorrow"
alias cu="yay -Syy && yay -Pu"
alias mo=mimeopen
alias u="yay -Syu --noconfirm"
alias t="todoist"

if [[ -f /usr/share/zsh/share/antigen.zsh ]]; then
    source /usr/share/zsh/share/antigen.zsh
elif [[ -f /usr/share/zsh-antigen/antigen.zsh ]]; then
    source /usr/share/zsh-antigen/antigen.zsh
fi

antigen init ~/.antigenrc

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

# Enable Ctrl-x-e to edit command line
autoload -U edit-command-line
# Emacs style
zle -N edit-command-line
bindkey '^xe' edit-command-line
bindkey '^x^e' edit-command-line
# Vi style:
# zle -N edit-command-line
# bindkey -M vicmd v edit-command-line

eval "$(direnv hook $SHELL)"

# Base16 Shell
BASE16_SHELL="$HOME/.config/base16-shell/"
[ -n "$PS1" ] && \
    [ -s "$BASE16_SHELL/profile_helper.sh" ] && \
    eval "$("$BASE16_SHELL/profile_helper.sh")"

if alias run-help > /dev/null 2>&1; then
    unalias run-help
    autoload run-help
fi
