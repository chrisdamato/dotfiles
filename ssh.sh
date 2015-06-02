function ssh() { 
    AARGH=""
    while (( $# )); do 
        if [[ "$1" == "pd" ]] ; then
            OPT="-o 'proxycommand ssh hq nc %h %p'"
        elif [[ "$1" =~ ^[0-9]+$ ]]; then
            AARGH="${AARGH} 10.18.3.$1"
        else
            AARGH="${AARGH} $1"
        fi
        shift
    done;
    SSH_COMMAND="/usr/bin/ssh $OPT ${AARGH}"
    echo $SSH_COMMAND
    eval $SSH_COMMAND
}
