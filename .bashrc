# Prompt
#export PS1="${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\t \[\033[01;35m\]^\[\033[0;31m\]-\[\033[01;35m\]^\[\033[01;34m\] \w \[\033[0;31m\]#\# \[\033[01;34m\]\$\[\033[00m\] "

# $PATH
## Scripts
export PATH=$PATH:~/scripts

# Alias to multiple ls commands
alias la='ls -Al'               # show hidden files
alias lx='ls -lXB'              # sort by extension
alias lk='ls -lSr'              # sort by size
alias lc='ls -lcr'      # sort by change time
alias lu='ls -lur'      # sort by access time
alias lr='ls -lR'               # recursive ls
alias lt='ls -ltr'              # sort by date
alias lm='ls -al |more'         # pipe through 'more'

# tmux
alias ta='tmux attach'
# alias for apt
alias ai='sudo apt-get install'	# Install things

export TERM="xterm-256color"

# Custom Commands
alias yolo='sudo $(history -p \!\!)'

export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting

# Rust
source $HOME/.cargo/env
export RUST_SRC_PATH="$HOME/.rust/src"

# History size
export HISTSIZE="INFINITE"

# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
export PATH="$PATH:$HOME/.rvm/bin"
