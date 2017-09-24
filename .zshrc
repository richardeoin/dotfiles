# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="bullet-train"

# Display context when user is not richard
BULLETTRAIN_CONTEXT_SHOW=true
DEFAULT_USER=richard

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git zshmarks)

# User configuration

export PATH="/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games:/home/richard/scripts:/home/richard/scripts"
# export MANPATH="/usr/local/man:$MANPATH"

source $ZSH/oh-my-zsh.sh

source ~/scripts/z/z.sh

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/dsa_id"

# scripts
export PATH=$PATH:~/scripts
# richard
export CDPATH=$CDPATH:/richard

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# Reload this
alias reload=". ~/.zshrc && echo 'ZSH config reloaded from ~/.zshrc'"
alias zshrc="emacs ~/.zshrc && reload"

# install things
alias ai='sudo apt-get install'
alias au='sudo apt-get update'
# emacs on the command line is always.. emacs on the command line
alias emacs='emacs -nw'
# except when it isn't
alias emacs-gui='emacs'
# git
alias clone='git clone'
# make me a sandwich
alias yolo='sudo $(fc -ln -1)'

#udev
alias udev-reload='sudo udevadm control --reload-rules && udevadm trigger'

export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting

# Rust
source $HOME/.cargo/env
export RUST_SRC_PATH="$HOME/.rust/src"

alias awrde='env WINEPREFIX="/home/richard/.wine" wine C:\\Program\ Files\ \(x86\)\\AWR\\AWRDE\\11\\MWOffice.exe -f\ \"MWO-228,\ VSS-350,\ ACE-100,\ ANA-001,\ APL-100,\ APL-110,\ APL-150,\ RDR-100,\ XEM-001\"'
alias awrde_help='env WINEPREFIX="/home/richard/.wine" wine C:\\windows\\command\\start.exe /Unix /home/richard/.wine/dosdevices/c:/users/richard/Start\ Menu/Programs/AWRDE\ 11\ \(64bit\)/AWR\ Design\ Environment\ Help.lnk'
alias awrde_gettingstarted='env WINEPREFIX="/home/richard/.wine" wine C:\\windows\\command\\start.exe /Unix /home/richard/.wine/dosdevices/c:/users/richard/Start\ Menu/Programs/AWRDE\ 11\ \(64bit\)/Getting\ Started.lnk'

# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
export PATH="$PATH:$HOME/.rvm/bin"
