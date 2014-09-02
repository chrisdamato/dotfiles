#!/bin/bash
# damato new uberrc
#
# profile-type settings for non-interactive shells first
# http://google-styleguide.googlecode.com/svn/trunk/shell.xml

_VV=$(stat -c %Z ~/.bashrc)

[[ ! "$_VV" == "$_V" ]] && unset _BASHRC

if [[ ! $_BASHRC =~ "1" ]] ; then
  # profile-type settings
  export _V=$_VV


  # this script replaces both .bashrc and .bash_profile
  # cleanup any traces of previous schemes
  for F in ~/.bash_profile ~/.bash_logout ~/.bashrc-cd ~/bashrc-cd; do
    [ -f $F ] && \mv $F $F.disabled -f > /dev/null
    done

  # if .bash_profile is not already a hard link to this file, make it so
  [ ! -f ~/.bash_profile ] && ln ~/.bashrc ~/.bash_profile -f

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
  export PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:$HOME/dotfiles

  for script in /etc/profile.d/*.sh ; do
          if [ -r $script ] ; then
                  . $script
  	fi
  done

  # Aliases and functions
  alias ls="ls -FC --color=always "
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
  # if you find yourself in a dark-on-light terminal
  alias bl='PS1="\[\e[0;31m\]\u\[\e[0;31m\]@\[\e[1;31m\]\h\[\e[m\] \[\e[1;32m\]$\[\e[m\] \[\e[0;30m\]"'

#  function ssh() {
#    # if "ssh host" is the first command in this window, exec ssh so window closes at logout
#    [[ $_N == 1 && $# == 1 ]] && exec ssh "$@" || ssh "$@";
#  }

  #prints hostname heading and output from commands below
  #example: sssh hostname 'command; command'
  function sssh(){ echo -e "\n\n-- $1 --\n\n"; ssh $1 eval ${@:2}; }
  #prints hostname and output on a single line
  #example: sss hostname 'command; command'
  function sss(){ echo -en "$1\e[16G"; ssh $1 eval ${@:2}; }
  #like sssh for multiple hosts. Example: sssh all_pdb 'command; command';
  function forssh(){ for H in ${!1}; do ( sssh $H ${@:2} ); done }
  #like sss for multiple hosts. Example: sssh all_pdb 'command; command';
  function forsss(){  for H in ${!1}; do ( sss $H ${@:2} ); done }
  # interactive commands on multiple hosts
  function multi() { while read -e -p "multi $ " R; do for H in $*; do echo -ne $H ":\t"; ssh $H eval $R; done; echo; done; }
  # smaller smarter version, use like for H in ...; do sshh commands; done
  function sshh ()  { echo -en "$H\t"; OUT=$( ssh -o ConnectTimeout=2 -o batchmode=yes "$H" eval ${@:1} ); [ "$OUT" ] && echo "$OUT" || echo -; }

  function bu() { cp $1 $1-$(date +%Y-%m-%d-%H-%M) -vba ;} # backup a file with a timestamp
#  function mu() { mv $1 $1-$(date +%Y-%m-%d-%H-%M) -v ;} # rename a file with a timestamp

  # indicate we have completed profile-type settings for next time
  export _BASHRC=1:$_BASHRC

fi # profile-type settings

# Exit if not interactive shell
[[ $- =~ "i" ]] || return

if [[ ! $_BASHRC =~ "2" ]] ; then
  # for interactive shells
  # Setup some environment variables.
  HISTCONTROL=ignoredups:ignorespace
  HISTTIMEFORMAT="%F %R "
  HISTIGNORE="&:[bf]g:exit"
  HISTSIZE=50000
  HISTFILESIZE=1000000
  # append to the history file, don't overwrite it
  shopt -s histappend
  shopt -s histverify
  shopt -s checkwinsize
  shopt -s no_empty_cmd_completion
  shopt -s checkhash
  shopt -s cdspell
  shopt -s extglob
  shopt -s globstar

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


  # update history record after each command
  # set title for supported terminals
  if [[ $TERM = linux ]]; then
  		PROMPT_COMMAND="_N=$(( $_N + 1 ));history -a"
  	else
		PROMPT_COMMAND='_N=$(( $_N + 1 ));history -a; echo -e "\033]0;$USER@$(hostname -s)\007"'
  	fi

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
  	function pscolor() {
  		# print hash-based color code for prompt
  		# no args clear color
  		[ -z "$1" ] && echo -en "\e[0m" && return
  		local col=(0\;31 0\;32 0\;33 0\;35 0\;36 0\;37 1\;31 1\;32 1\;33 1\;34 \
  			1\;35 1\;36 1\;37 4\;31 4\;32 4\;33 4\;34 4\;35 4\;36 4\;37)
  		local param=$1
  		set -- $( echo $* | cksum )
  		local index=$(( $1 % ${#col[*]} ))
  		if [[ "$param" == "damato" ]]; then
			echo -en "\e[0;34m"
			else
	  		echo -en "\e[${col[$index]}m"
			fi
  		}
  	function colorize() {
  		# print hash-based colored text
  		[[ -z "$@" ]] && echo -en "\e[0m" && return
  		local col=(0\;31 0\;32 0\;33 0\;35 0\;36 0\;37 1\;31 1\;32 1\;33 1\;34 \
  			1\;35 1\;36 1\;37 4\;31 4\;32 4\;33 4\;34 4\;35 4\;36 4\;37)
  		local param="$@"
  		set -- $( echo "$@" | cksum )
  		local index=$(( $1 % ${#col[*]} ))
  		echo -en "\e[${col[$index]}m${param}\e[0m\n"
  		}
  	NORMAL="\[\e[0;0m\]"
  	RED="\[\e[0;31m\]"
  	GREEN="\[\e[1;32m\]"

  else
  	function pscolor() { echo $1; }
  fi

  if [[ $EUID == 0 ]] ; then
    export PS1="\[\e[0m\w\n$RED\u$NORMAL@\[$(pscolor $HOSTNAME)\]\h$NORMAL $RED#$NORMAL "
  else
    export PS1="\[\e[0m\w\n\[$( pscolor $USER )\]\u$NORMAL@\[$(pscolor $HOSTNAME)\]\h$NORMAL $GREEN\$$NORMAL "
  fi


  # lists of servers
  all_a="a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 pdbweb"
  all_b="b1 b2 b3 b4 b5 b6"
  all_c="c1 c2 c3"
  all_d="d1 d2 d3 d4 d5 d6"
  all_f="f1 f2 f3 f4 f5 f6"
  all_k="k1 k2 k3 k4 k5 k6 k7 k8 k9 k10 k11 k12 k13 k14 k15 k16"
  weird_L="L27 L28 L29 "
  all_L="L4 L11 L24 L31 L32 L33 L34 L35 L36 L37 L38 L39 L41 L42 L43 L44 L45 L46"
  all_dp="dp1 dp2 dp3"
  all_db="db1 db2 db4 db5 db6"
  all_mail="mail4 mail5 mail6"
  all_hp="hp1 hp2 hp3 hp4 hp5 hp6 hp7 hp8"
  all_tools="pdb-a-linux-10 pdb-a-linux-11 pdb-a-linux-12 pdb-a-linux-13 pdb-b-linux-1 pdb-b-linux-2"
  all_wsa="wsa3 wsa5 wsa6 wsa8 wsa10 wsa11 wsa12 wsa13 wsa17 hudson.rcsb sutapa.rcsb buvna.rcsb chenghua.rcsb lihua.rcsb"
  all_kb="a4 b1 b2 b4 k5 k6 k11 k12 k13 k14 k15 k16"
  all_other="meerkat prion"
  all_pdb="$all_a $all_b $all_c $all_d $all_f $all_k $all_L $all_dp $all_db $all_hp $all_other"
  all_biomaps="biomaps juggernaut hugin gyges tyr vidar nikka nikkafs flail cottus nikkaweb bayes casegroup casegroup1 casegroup2 casegroup3 fenrir briareus munin myosin sysbio bologna"
  all_annot="buvna chenghua dicostanzo guanghua hudson jasmin lihua marina peisach persikov sdutta sekharan sutapa yuhe"
  all_DA="b5 b6 d1 d2 f5 f6 hp1 hp2 hp3 hp4"
  export K='ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDE50uAFWu5AsG1v61P7vS4WznxaSDmu7CnkfANCK+blvkly8PNr+zbLRgnbxEV8QvfIcll3/RpFjy8E3WEhV9VPewKkkj1WDuEcdJHjTitdDLI3iKSDzNJM/6na8jsZItvVg9HqYkm5drk6opmJU38hwcef56T2iye2DTmFrL7ZjLz+sPS4i2gizPKCXk3/c2EZNSZDHBomND/dkpE9Yu1JgrYuvju9XOSFGiziMtVgDnEnRMGq5D/J1z9oyMGpK81viKWhNt7nDhfrWb8nVluw0jsQk9tsslMI0+Td7RngIMDObcqjYU3zMGe57EP3A5kBwyZzbBWM4SgGD5P9ICX damato@rcsb.rutgers.edu'
  function sshpkey() { cat ~/.ssh/*pub | ssh $1 "cat - >> ~/.ssh/authorized_keys"; }
  alias pkey='echo $K >> ~/.ssh/authorized_keys'

  export LESS=-R

  # suppress the hostname output when rsync
  export RSYNC_RSH='ssh -o PermitLocalCommand=no'

  if [[ ! $EUID == 0 ]] ; then
    for CMD in systemctl journalctl nmap su service vim apt-get yum dpkg rpm chmod chown mount umount reboot fdisk parted ip pacman iptables find poweroff shutdown reboot find; do
      alias $CMD="sudo $CMD"
      done
    fi
  alias please='eval "sudo $(fc -ln -1)"'

  # Press <escape> then <space> to sudo !!
  bind '"\e\ ":"\C-k \C-usudo !!\n"'

  function say() { mplayer -volume 100 "http://translate.google.com/translate_tts?tl=en&q=$*"; }
  function ff() { find . -type f -iname "*$1*" -printf "\"%p\"\n"; }
  function fd() { find . -type d -iname "*$1*" -printf "\"%p\"\n"; }
  alias d='cd ~/Dropbox/Documents/divorce'

# winetricks support
  prefix() {
    export WINEPREFIX="$HOME/.local/share/wineprefixes/$1"
  }

  goc() {
    cd $WINEPREFIX/drive_c
  }

  lsp() {
    ls $* $HOME/.local/share/wineprefixes
  }

  run() {
    prefix $1
    goc
    wine cmd /c run-$1.bat
    }

  export _BASHRC=2:$_BASHRC
  if [[ "$DISPLAY" = ":0" ]]; then  xhost +si:localuser:root > /dev/null; fi
  # suppress console spam from evince
  export NO_AT_BRIDGE=1

  function clearpdf() { IN=$1; OUT="${IN// /-}"; D=$(mktemp -d); convert -verbose -density 200x200 "$IN" "$D/page.png"; convert "$D/*.png" clear-$OUT && mv "$IN" "$IN.protected" -v && mv "clear-$OUT" "$OUT" -v; echo $OUT; }

  function smallpdf() {     IN=$1;     OUT="${IN// /-}";     D=$(mktemp -d); convert -verbose -density 150x150 -quality 70 "$IN" "$D/page.jpg"; convert "$D/*.jpg" clear-$OUT && mv "$IN" "$IN.protected" -v && mv "clear-$OUT" "$OUT" -v; echo $OUT; ls -lh "$IN.protected" "$OUT"; }

  function ev() { while [ -s "$1" ]; do evince "$1" < /dev/null &> /dev/null & disown -h &> /dev/null; shift; done }

 # doing this in .Xresources instead
 #  alias urxvt="urxvt  +sb -fg white -bg black -fn  \"xft:Droid Sans Mono:pixelsize=13:antialias=true\" +ptab -letsp 0 -fade 30 -keysym.Home \"\\033[1~\" -keysym.End \"\\033[4~\""

  alias gid='dig +noall +answer -x' # reverse dns lookup

  for C in start stop status enable disable restart; do alias $C="systemctl $C"; done

  # show active torrents of matching status
  # ex: see idle seeding
  function see { IFS="|";transmission-remote -l |cut -c58-|grep -iE "^$*"|sed 's/ \+/\t/'; }

  function moviemerge { ffmpeg -f concat -i <(for F in ${@:2}; do echo "file $PWD/$F"; done ) -c copy "$1"; }
  function to { echo -e "Moving: ${@:2}\nTarget: $1\nLink from: /media/incoming"; if test -d "$1" ; then mv "$2" "$1" -v ; ln "$1/$2" /media/incoming -sv; fi; }
  function mto { echo -e "Moving: ${@:2}\nTarget: $1\nLink from: /media/incoming"; if test -d "$1" ; then for F in ${@:2}; do mv "$F" "$1" -v ; ln "$1/$F" /media/incoming -sv; done; fi; }
  
  alias yuma="yum --enablerepo=\*"
  alias yyuma="yum -y --enablerepo=\*"
  alias yyum="yum -y"

  echo "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

  HELLO="$( lsb_release -ds | fmt )"
  [[ -z $HELLO ]] && HELLO=$(cat /etc/redhat-release)
  [[ -z $HELLO ]] && HELLO="I'm confused"
  colorize $HELLO
  date

  # http://is.muni.cz/www/xsiska2/2014/08/01/get-faster-in-bash.html
  bind space:magic-space

  fi



