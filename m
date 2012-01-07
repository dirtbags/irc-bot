#! /bin/sh

if [ ! -p ../fifo ]; then
  echo "You need to be in a forum directory."
  exit
fi

forum=${PWD##*/}
nick=${nick:-?}

privmsg () {
  (
    flock 1
    now=$(date '+%Y-%m-%d %H:%M')
    case "$1" in
        //*)
            printf "PRIVMSG %s :%s\n" "$forum" "${1#/}" > ../fifo
            echo "$now <$nick> ${1#/}"
            ;;
        \:*)
            printf "PRIVMSG %s :\001ACTION %s\001\n" "$forum" "${1#:}" > ../fifo
            echo "$now * $nick ${1#:}"
        ;;
         /*)
            cmd=${1%% *}
            args=${1#* }
            case "${1%% *}" in
                /msg)
                    echo "PRIVMSG ${args%% *} :${args#* }" > ../fifo
                    ;;
                  *)
                    echo "${1#/}" > ../fifo
                    ;;
            esac
        ;;
      *)
        printf "PRIVMSG %s :%s\n" "$forum" "$1" > ../fifo
        echo "$now <$nick> $1"
        ;;
    esac
  ) >> log
}

if [ "$*" ]; then
  privmsg "$*"
else
  while true; do
    printf "$forum: "
    read -r msg || break
    privmsg "$msg"
  done
fi
