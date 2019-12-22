echo "Racionais, Capítulo 4, Versículo 3"

export ZSH="/home/guilhxavier/.oh-my-zsh"
export PATH="$PATH:/home/guilhxavier/development/flutter/bin"
export PATH="$PATH:/opt/android-studio/bin"

ZSH_THEME="spaceship"

plugins=(git)

source $ZSH/oh-my-zsh.sh

#----------------------
# Aliases
#----------------------

# Comandos tri
alias reload="source ~/.zshrc"
alias _="sudo"
alias rd="rm -rf"
alias tarik="tar -zxvf"

#Android Studio
alias android="studio.sh"

#Fortnite vpn
alias fortnibas="sudo openfortivpn vpnclient01.cwi.com.br -u guilherme.xavier -p Mav@@2019 --trusted-cert e876c389be2eb1558f07aa347aadeb716d223f7a12b425903934a77dfb07dae0"

# Navigation
alias home="cd ~"
alias prev="cd .."
alias prev..="cd ../.."
alias prev...="cd ../../.."

# Important directories
alias crescer="cd ~/workspace/crescer"

# npm
alias ni="npm install"
alias nu="npm uninstall"
alias nup="npm update"

# Generate .gitiginore
alias gitignore-node="curl http://gitignore.io/api/node,visualstudiocode >> ./.gitignore"
alias gitignore-java="curl http://gitignore.io/api/java,intellij >> ./.gitignore"
alias gitignore-flutter="curl https://www.gitignore.io/api/dart,flutter >> ./.gitignore"

SPACESHIP_PROMPT_ORDER=(
	dir
  git           # Git section (git_branch + git_status)
  exec_time     # Execution time
  line_sep      # Line break
  char          # Prompt character
)
SPACESHIP_USER_SHOW=always
SPACESHIP_TIME_SHOW=true
SPACESHIP_PROMPT_ADD_NEWLINE=false
SPACESHIP_PROMPT_SEPARATE_LINE=false
SPACESHIP_DIR_TRUNC=1
SPACESHIP_DIR_TRUNC_PREFIX="../../"
SPACESHIP_CHAR_SYMBOL="λ"
SPACESHIP_CHAR_SUFFIX=" "
### Added by Zplugin's installer
source '/home/guilhxavier/.zplugin/bin/zplugin.zsh'
autoload -Uz _zplugin
(( ${+_comps} )) && _comps[zplugin]=_zplugin
### End of Zplugin installer's chunk
zplugin light zdharma/fast-syntax-highlighting
zplugin light zsh-users/zsh-autosuggestions
zplugin light zsh-users/zsh-completions
