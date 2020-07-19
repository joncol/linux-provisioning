ZDOTDIR=~/.config/zsh

export EDITOR=vim

# export LC_ALL=C # To get rid of `xmessage` warning

export PATH="$PATH:$HOME/.cargo/bin:$HOME/.local/bin"
export PATH="$PATH:$HOME/.rvm/bin"

# n/npm paths
export N_PREFIX=$HOME/.n
if [[ -d "$N_PREFIX/bin" ]]; then
    export PATH=$N_PREFIX/bin:$PATH
fi

npm_bin=$(npm bin 2>/dev/null)
if [[ -d ${npm_bin} ]]; then
    export PATH=${npm_bin}:$PATH
fi

npm_global_bin=$(npm bin --global 2>/dev/null)
if [[ -d ${npm_global_bin} ]]; then
    export PATH=${npm_global_bin}:$PATH
fi

export FZF_DEFAULT_OPTS='--bind ctrl-f:page-down,ctrl-b:page-up --color fg:124,hl:202,fg+:214,bg+:52,hl+:231 --color info:52,prompt:196,spinner:208,pointer:196,marker:208'
export FZF_CTRL_R_OPTS="--preview 'echo {}' --preview-window down:3:hidden:wrap --bind '?:toggle-preview' --bind 'ctrl-y:execute-silent(echo -n {2..} | xsel -b)+abort' --header 'Press CTRL-Y to copy command into clipboard'"

export ANSIBLE_NOCOWS=1
