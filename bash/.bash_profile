
source ~/.exports

alias clear-pyc='find . -type f -name "*.pyc" -delete'
alias pgrep='pgrep -fl'

# git aliases
alias gpull="git stash -k && git pull --rebase && git stash pop"
alias gb="git branch"
alias gd="git diff"
alias co="git checkout"
alias st="git status"
alias pull="git pull"
alias ggrep="git grep"
