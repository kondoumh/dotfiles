#alias ls='ls -G'
#alias ll='ls -lG'
#alias la='ls -aG'

export PATH=${PATH}:$HOME/.nodebrew/current/bin:/usr/local/sbin:$HOME/go/bin:$HOME/.cargo/bin
export NODE_PATH=$(npm root -g)
source ~/.cargo/env
