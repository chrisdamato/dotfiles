#!/bin/bash
# set -x # debug!
# THIS="$(basename -- $0 )"
###       _                 _                _    
###    __| |__ _ _ __  __ _| |_ ___       __| |_  
###   / _` / _` | '  \/ _` |  _/ _ \  _  (_-< ' \ 
###   \__,_\__,_|_|_|_\__,_|\__\___/ (_) /__/_||_|
###                                               
###   
# 2015-05 using this as /etc/profile.d/damato.sh these days

# installation notes:
#   arch: copy to /etc/profile.d, remove ~/.bashrc ~/.bash_profile

# profile-type settings

    # The personal initialization file, executed for login shells, NOT every new terminal
    # NO OUTPUT TO TERMINAL IN THIS SECTION

    # added 2013-11-14 per http://lists.debian.org/debian-devel/2002/07/msg00259.html
    export XAUTHORITY=$HOME/.Xauthority

    # Functions to help us manage paths.    Second argument is the name of the
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

    # changes 
    alias ..pull="curl scratch.chrisdamato.com/damato.sh > /etc/profile.d/damato.sh # pull"
    # alias ..push="echo cat $BASH_SOURCE \| ssh damato@scratch.chrisdamato.com \'cp damato.sh damato.sh.\$\(date +%s\) \&\& cat \> damato.sh \&\& cp damato.sh /var/damato/damato.sh -vb\' # push"
    alias ..push='rsync /etc/profile.d/damato.sh damato@scratch.chrisdamato.com:/var/damato/damato.sh --backup --suffix .$(date +%s) -v # 2015-08-11'
    alias ..s="source $BASH_SOURCE"
    alias ..e="${EDITOR:-vim} $BASH_SOURCE"
    alias ..add-rpmfusion-repos='yum localinstall --nogpgcheck http://download1.rpmfusion.org/free/fedora/rpmfusion-free-release-$(rpm -E %fedora).noarch.rpm http://download1.rpmfusion.org/nonfree/fedora/rpmfusion-nonfree-release-$(rpm -E %fedora).noarch.rpm'
    alias ..key='test -d ~/.ssh || mkdir ~/.ssh; echo $PUBKEY >> ~/.ssh/authorized_keys; sort -o ~/.ssh/authorized_keys -u ~/.ssh/authorized_keys'

    # Set the initial path
    pathprepend $HOME/dotfiles

    # these days we are mostly placing this in /etc/profile.d as damato.sh -- 2015-03
    # load system profile scripts
    #    for script in /etc/profile.d/*.sh ; do [ -r $script ] && . $script ; done


    # want to support Linux, FreeBSD, and Cygwin for the basics at least
    SYSTEM_TYPE="$(uname)";
    SYSTEM_TYPE="${SYSTEM_TYPE,,}"
    if [[ "$SYSTEM_TYPE" =~ cygwin ]] ; then
        export CYGWIN=YES
        alias sudo=exec
	fi
    [[ "$SYSTEM_TYPE" =~ freebsd ]] && FREEBSD=YES
    if [[ "$SYSTEM_TYPE" =~ linux ]]; then
        LINUX=YES

    fi



    # Aliases and functions
    alias ls="ls -FC --color=auto" # linux and cygwin
    [ $FREEBSD ] && alias ls="ls -FCG" # FreeBSD
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
    alias lsblk='lsblk --output NAME,TYPE,SIZE,FSTYPE,MOUNTPOINT,LABEL,PARTLABEL,VENDOR'
    alias pkill='pkill -e -9'
    alias screen="screen -L"
    alias scrot='scrot "$HOME/Downloads/%Y-%m-%d_$wx$h_scrot.png" -s -e "eog \$f"'
    function ..list () {
	sudo masscan -p${1:-139} --rate 512 --wait 1 10.18.3.0/24 2>/dev/null | \
        cut -d' ' -f6 | \
        parallel --gnu --timeout 2 'nmblookup -A {1} | perl -ne '"'"'/Looking up status of ([0-9.]+)/ && print "$1\t"; /\s+(\S+)\s+<00>\s-\s+[BM]/ && print "$1\n" '"'"' ' | \
	ipsort | tee ~/list
        wc -l < ~/list # windows PCs up. takes 3 sec
    }

    export PUBKEY="ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDZ96eieFTn4u4YwugIcSAyvyQJKc/eP638phl8rMdqYd6C+1sS18ibXV4NZ1flheWAfyIj2q+WYoe5KpkqZsa7rRWfx2QUzgcYa09ziCM/p4p+SYicrc8BPyT4nh1d6HEQ+8ArDDfKW48YsirmL0Axmzluj76BSGshnWhViyWOQXY18o4d6xe00pHepQu7UcMiV4zIhhglXjILfK3qtYtZJr5jdaz3VDnU5FRtfKh/n5S3NIVzHV57iU/xAums71UtLkdxfXljbd1aGCclXoQbegmiv8mvxBM8a1gqTqustRwOArQ9EitIX7zLhWnEytKuw78YE5/HRrPJ72yV9S0b"

    # NO OUTPUT TO TERMINAL IN THIS SECTION

    # don't like systemctl's auto paging feature
    export SYSTEMD_PAGER=

# if not interactive end here
[[ ! "$-" =~ "i" ]] && return

    # remove any background colors from dir listings
    eval $(dircolors|sed -r 's/;?4[0-9];?//g')

    # just print a blank line if we are running interactively
    echo
     
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

    alias n="nmcli"
    alias f="firewall-cmd"
    alias pw='cat ~/pw ~/users ~/nbt* ~/keys|grep -iE'

    alias grep='grep --color=auto --binary-files=without-match  --directories=skip'


    # backup a file with a timestamp
    function bu() { cp $1 $1-$(date +%Y-%m-%d-%H-%M) -vba ;} 

    # print a multiline label on dymo - not portable
    function label() { echo -e "\nEnter your multi-line label text. End with Ctrl+D\n";cat |sed 's/$/\\n/g'| tr -d \\n > /tmp/foo; ( glabels-3-batch /ext/Dropbox/Boro/dymo-plain-11pt-droid-serif.glabels -i /tmp/foo -o >(lp -n1) ) }

    # systemd shortcuts
    for C in start stop status enable disable restart; do alias $C="systemctl $C"; done

    # permit root to use X display
    if [[ "$DISPLAY" = ":0" ]]; then    xhost +si:localuser:root > /dev/null; fi

    # automatic sudo for common admin tasks
    if [[ ! $EUID == 0 ]] && which sudo &> /dev/null ; then
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


	function _colorize() {
		# print hash-based colored text
		[[ -z "$@" ]] && echo -en "\e[0m" && return
		echo -en "$(colr $*)${*}$(colr)";
		}

    else
         #no color terminal 
        function _colorize() { echo -n "$@"; }
        function pscolor() { echo -n "$@"; }
        
    fi # color terminal

    export PS1="\n\! \w\n\[\e[1;\$( [[ \$USER == root ]] && echo -n 31 || echo -n 32 )m\]\u\[\e[0m\]@$(pscolor $(hostname -s ))\$ "

    # actions before displaying prompt
    # set terminal window title
    function term_title() { echo -ne "\033]0;$USER@$HOSTNAME:$PWD\007"; }
    # append to history
    export PROMPT_COMMAND="history -a; term_title"
#    trap 'echo -ne "\033]0;$BASH_COMMAND\007"' DEBUG

    function ..open() {
        sudo masscan -p${1:-139} --rate 512 --wait 1 ${2:-10.18.3.0/24} 2> /dev/null | 
        cut -d' ' -f6 | ipsort
    }

    function ipaddresses() {
        if [[ $FREEBSD ]] ; then
            for I in $(ifconfig -l -u inet|tr ' ' '\n'|grep -v lo); do 
                echo $I $(ifconfig $I inet | grep -Eo "inet [0-9.]+" | cut -d' ' -f2)  
                done
        elif [[ $CYGWIN ]] ; then
            netsh i i sh con | grep "IP Address:" | grep -v "127.0.0.1" | grep -oE '([0-9]{1,3}\.?){4}'
        else
            ip -o -f inet a |grep -v '127\.0\.0\.1'|tr '/' ' '|awk '{print $2, $4}'
        fi
    }

    function welcome() { 
        [ -f /etc/redhat-release ] && echo $(_colorize $(cat /etc/redhat-release) )
        for A in $(for O in i m o p r s ; do uname -$O; done | sort | uniq); do echo -n $(_colorize $A) " "; done; echo
        # print ip addresses (non-lo) if found with ip command
        # formerly: grep "inet\ .*[^l][^o]$" <( ip a 2>/dev/null ) | tr '/' ' ' | awk '{print $NF" "$2}'
	echo "This: $BASH_SOURCE"
        hostname
        ipaddresses
        if [ $LINUX ] || [ $FREEBSD ]; then
            echo
            fi
        if [ $CYGWIN ]; then
            if [ ! -f /usr/bin/apt-cyg ]; then
                echo '[31mcygwin first run notes: [0m'
                echo 'install apt-cyg etc'
                echo
                echo '  lynx -source rawgit.com/transcode-open/apt-cyg/master/apt-cyg > apt-cyg && install apt-cyg /bin'
            fi
            if ! grep --quiet "$PUBKEY" $HOME/.ssh/authorized_keys; then
                echo
                echo Install public key for ssh:
                echo 31m..key[0m
                echo
            fi
            echo '[32mcygwin notes:[0m'
            echo
            echo '    apt-cyg install wget curl vim'
            echo '    apt-cyg install openssh && ssh-host-config -y -w "$(date)" && ssh-user-config -y -p && net start sshd'
            echo
            find /etc/profile.d -name "0*.sh"
            fi
        }
    
    welcome
# end of .bashrc interactive settings
