#!/bin/bash
# set -x # debug!
# damato new uberrc - LIGHT VERSION
# 2015-02
# link as .bashrc .bash_profile and .bash_logout

# THIS="$(basename -- $0 )"

#THIS_REALLY="$(test -L "$0" && readlink "$0" || echo "$0")"

if [ "$THIS" = ".bash_logout" ] ; then
    # The individual login shell cleanup file, executed when a login shell exits
    return
    fi

# profile-type settings

  # The personal initialization file, executed for login shells, NOT every new terminal
  # NO OUTPUT TO TERMINAL IN THIS SECTION

  # added 2013-11-14 per http://lists.debian.org/debian-devel/2002/07/msg00259.html
  export XAUTHORITY=$HOME/.Xauthority

  # Functions to help us manage paths.  Second argument is the name of the
  # path variable to be modified (default: PATH)
  pathremove () {
    local IFS=':'
    local NEWPATH
    local DIR
    local PATHVARIABLE=${2:-PATH}
    for DIR in ${!PATHVARIABLE} ; do
      if [ "$DIR" != "$1" ] ; then
        NEWPATH=${NEWPATH:+$NEWPATH:}$DIR
      fi
    done
    export $PATHVARIABLE="$NEWPATH"
  }
  pathprepend () {
    pathremove $1 $2
    local PATHVARIABLE=${2:-PATH}
    export $PATHVARIABLE="$1${!PATHVARIABLE:+:${!PATHVARIABLE}}"
  }
  pathappend () {
    pathremove $1 $2
    local PATHVARIABLE=${2:-PATH}
    export $PATHVARIABLE="${!PATHVARIABLE:+${!PATHVARIABLE}:}$1"
  }


  # Set the initial path
  pathprepend $HOME/dotfiles

  # load system profile scripts
  for script in /etc/profile.d/*.sh ; do [ -r $script ] && . $script ; done

  # Aliases and functions
  [ $(uname) = FreeBSD ] && alias ls="ls -FCG" || alias ls="ls -FC --color=auto"
  alias ll="ls -lArt"
  alias lss='ls -rhS'
  alias lst='ls -rht'
  alias la="ls -a"
  alias l='ls -lah'
  alias lla="ls -lah"
  alias ..='cd ..'
  alias ...='cd ../..'
  alias ....='cd ../../..'
  alias .....='cd ../../../..'
  alias dog='sed "/ *#/d; /^ *$/d"' # cat without comments and whitespace
  alias ipsort='sort -n -t . -k 1,1 -k 2,2 -k 3,3 -k 4,4 '

  # NO OUTPUT TO TERMINAL IN THIS SECTION



# if not interactive end here
[[ ! "$-" =~ "i" ]] && return

  # if .bash_profile is not already a hard link to this file, make it so
  [ ! -f ~/.bash_profile ] && ln ~/.bashrc ~/.bash_profile -f

  # just print a blank line if we are running interactively
  echo
  #[[ "${BASH_SOURCE[0]}" != "${0}" ]] && echo "script ${BASH_SOURCE[0]} is being sourced ..."
   
  # THIS SECTION IS FOR INTERACTIVE SHELLS

  HISTCONTROL=ignoredups:ignorespace
  HISTTIMEFORMAT="%F %R "
  HISTIGNORE="&:[bf]g:exit"
  HISTSIZE=50000
  HISTFILESIZE=1000000

  shopt -s histappend # append to the history file, don't overwrite it
  shopt -s histverify
  shopt -s checkwinsize
  shopt -s no_empty_cmd_completion
  shopt -s checkhash
  shopt -s cdspell
  shopt -s extglob

  # READLINE settings 
  # note to self: get keycode with echo <Ctrl+v><key>
  bind '"\e[6~":end-of-history' # PgDn
  bind '"\e[5~":dynamic-complete-history' # PgUp
  bind 'set blink-matching-paren on'
  bind 'set completion-ignore-case on'
  bind 'set completion-map-case on'
  bind 'set disable-completion off'
  bind 'set expand-tilde off'
  bind 'set horizontal-scroll-mode off'
  bind 'set mark-directories on'
  bind 'set mark-symlinked-directories off'
  bind 'set match-hidden-files on'
  bind 'set prefer-visible-bell on'
  bind 'set show-all-if-ambiguous off'
  bind 'set show-all-if-unmodified off'
  bind 'set skip-completed-text off'
  bind 'set bell-style none'
  bind 'set comment-begin #'
  bind 'set completion-prefix-display-length 2'
  bind 'set completion-query-items 6000'
  bind '"\e\ ":"\C-k \C-usudo !!\n"'
  bind '"\e[1;5D": backward-word'
  bind '"\e[1;5C": forward-word'
  # Press <escape> then <space> to sudo !!
  bind '"\e\ ":"\C-k \C-usudo !!\n"'

  # functions and aliases for convenience
  alias please='eval "sudo $(fc -ln -1)"'
  alias gid='dig +noall +answer -x' # reverse dns lookup

  alias yuma="yum --enablerepo=\*"
  alias yyuma="yum -y --enablerepo=\*"
  alias yyum="yum -y"

  # backup a file with a timestamp
  function bu() { cp $1 $1-$(date +%Y-%m-%d-%H-%M) -vba ;} 

  # systemd shortcuts
  for C in start stop status enable disable restart; do alias $C="systemctl $C"; done

  # automatic sudo for common admin tasks
  if [[ ! $EUID == 0 ]] ; then
    for CMD in systemctl journalctl nmap su service vim apt-get yum dpkg rpm chmod \
        chown mount umount reboot fdisk parted ip pacman iptables find poweroff \
	shutdown reboot find systemctl journalctl; do
      alias $CMD="sudo $CMD"
      done
    fi
  
  function sshpkey() { cat ~/.ssh/*pub | ssh $1 "cat - >> ~/.ssh/authorized_keys"; }
  alias pkey='echo $K >> ~/.ssh/authorized_keys'

  # fast find
  function ff() { find . -type f -iname "*$1*" -printf "%AY-%Am-%Ad-%AI-%AM\t%k\t\"%p\"\n"; }
  function fd() { find . -type d -iname "*$1*" -printf "\"%f\"\n"; }

  if tput colors &> /dev/null; then # this terminal does color
  	# colored man pages thanks Arch wiki
	# http://unix.stackexchange.com/a/7431
	man() {
		# md, me = bold (red)
		# se, so = highlight
		# us, ue = underlined
		env \
		LESS_TERMCAP_md=$'\E[01;31m' \
		LESS_TERMCAP_me=$'\E[0m' \
		LESS_TERMCAP_se=$'\E[0m' \
		LESS_TERMCAP_so=$'\E[01;37;42m' \
		LESS_TERMCAP_us=$'\E[04;33m' \
		LESS_TERMCAP_ue=$'\E[0m' \
		GROFF_NO_SGR=1 \
		man "$@"
	}

  	function colr() {
  		# get hash-based color code for parameter (or reset if none)
  		[[ -z "$@" ]] && echo -en "\e[0m" && return
		# list of acceptable color codes
  		local COLOR_LIST=(0\;32 0\;33 0\;35 0\;36 1\;32 1\;33 \
  			1\;35 1\;36 4\;32 4\;33 4\;34 4\;35 4\;36 )
		local COUNT="${#COLOR_LIST[*]}"
		local HASH="$( echo "$@" | cksum | cut -d' ' -f1 )"
		local INDEX="$(( $HASH % $COUNT))"
  		local CODE="\e[${COLOR_LIST[$INDEX]}m"
		# overrides
		[[ ":damato:chris:" =~ ":$@:" ]] && CODE="\e[0;34m";
		[[ ":root:Administrator:" =~ ":$@:" ]] && CODE="\e[1;31m";
		echo -en "$CODE"
  		}

  	function pscolor() {
  		# wrap param in hash-based color codes escaped for use in PS1 prompt
		echo -en "\[$(colr $*)\]${*}\[$(colr)\]";
		}


  	function colorize() {
  		# print hash-based colored text
  		[[ -z "$@" ]] && echo -en "\e[0m" && return
		echo -en "$(colr $*)${*}$(colr)";
  		}

  else
     #no color terminal 
    function colorize() { echo -n "$@"; }
    function pscolor() { echo -n "$@"; }
    
  fi # color terminal

  export PS1="\n\! \w\n$(pscolor $USER)@$(pscolor $HOSTNAME)\$ "
  export PROMPT_COMMAND="history -a"
#  trap 'echo -ne "\033]0;$BASH_COMMAND\007"' DEBUG

  
  [ -f /etc/redhat-release ] && echo $(colorize $(cat /etc/redhat-release) )
  for A in $(for O in i m o p r s ; do uname -$O; done | sort | uniq); do echo -n $(colorize $A) " "; done
  echo

# end of .bashrc interactive settings
