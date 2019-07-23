export ZSH="/home/guilhxavier/.oh-my-zsh"

ZSH_THEME="simple"

SPACESHIP_NVM_SHOW="true"
ENABLE_CORRECTION="true"

COMPLETION_WAITING_DOTS="true"

plugins=(
	git
	zsh-syntax-highlighting
	zsh-autosuggestions
	zsh-nvm
)

source $ZSH/oh-my-zsh.sh

source /home/guilhxavier/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
fpath=($fpath "/home/guilhxavier/.zfunctions")

autoload -U promptinit; promptinit
prompt spaceship
fpath=($fpath "/home/guilhxavier/.zfunctions")
