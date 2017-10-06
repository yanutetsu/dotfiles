# EmacsでPATHを通した時にerrorになるためコメントアウト
# # コマンドプロンプトでCtrl+sを使えるようにする
# stty stop undef

# 履歴
export HISTISZE=100000
export HISTFILESIZE=100000
export HISTCONTROL=ignoredups:ignorespace:erasedups

export LESS='-R'
export LESSOPEN='| /usr/local/Cellar/source-highlight/3.1.8_2/bin/src-hilite-lesspipe.sh %s'

alias vi="vim"

alias ls="ls -FG"
alias la="ls -a"
alias ll="ls -l"
alias lal="ls -la"
alias lla="ls -la"

alias g="git"
alias gg="git graph"
alias ggg="git graphall"
alias gs="git status"

alias t="tig"
alias ta="tig --all"

source ~/.git-completion.bash

# pyenv
export PYENV_ROOT=$HOME/.pyenv
export PATH=$PYENV_ROOT/bin:$PATH
eval "$(pyenv init -)"

# Go
export GOROOT=`go env GOROOT`
export GOPATH=$HOME/go
# export GOPATH=$HOME/projects/jtb-agent-account-system/server/src:$GOPATH # for prj-jtb
export PATH=$GOPATH/bin:$PATH
export PATH=~/go_appengine:$PATH # コレなんだっけ？ ←これないとgbコマンドがまともに動かない

# JTB環境設定
export PATH=~/projects/jtb-agent-account-system/server:$PATH

# EPARK環境設定
# export PATH=~/procjects/epark/serverapp:$PATH
# export GOROOT=/usr/local/opt/go/libexec

export PATH=~/google-cloud-sdk/bin:$PATH

# mvn
export PATH=/usr/local/apache-maven-3.3.9/bin:$PATH
export JAVA_HOME=`/usr/libexec/java_home -v 1.7`
export PATH=$JAVA_HOME:$PATH

export PATH=~/appengine-java-sdk-1.9.40/bin:$PATH

# プロンプトを可愛くする
PS1="[\u@\w]\\n(*'-') < "

# display a git branch
function parse_git_branch {
    git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ (\1)/'
}
function promps {
    local BLUE="\[\e[1;34m\]"
    local RED="\[\e[1;31m\]"
    local GREEN="\[\e[1;32m\]"
    local YELLOW="\[\e[1;33m\]"
    local WHITE="\[\e[00m\]"
    local GRAY="\[\e[1;37m\]"

    case $TERM in
        xterm*) TITLEBAR='\[\e]0;\W\007\]';;
        *)      TITLEBAR="";;
    esac
    local BASE="\u@\h"
    # PS1="${TITLEBAR}${GREEN}${BASE}${WHITE}:${BLUE}\W${GREEN}\$(parse_git_branch)${BLUE}\$${WHITE}"
    PS1="[\u@${BLUE}\w${WHITE}]${YELLOW}\$(parse_git_branch)${WHITE}\\n${GREEN}(*'-') < ${WHITE}"
}
promps


[ -f ~/.fzf.bash ] && source ~/.fzf.bash

# # share bash history
# function share_history {
#     history -a
#     history -c
#     history -r
# }
# PROMPT_COMMAND='share_history'
# shopt -u histappend
# export HISTSIZE=9999
