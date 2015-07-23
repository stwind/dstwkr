#!/bin/sh
ERL=erl
COOKIE=dstwkr
HOST=127.0.0.1
CONFIG=priv/app.config
LIBS_DIR=deps
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
BASE_HANDOFF_PORT=9210

while getopts "fn:" arg; do
    case $arg in
        f)
            FRESH=1
            ;;
        n)
            NAME=$OPTARG
            ;;
        *)
            ;;
    esac
done

if [[ ! $NAME =~ ^-?[0-9]+$ ]]; then
    echo "ERROR: name should be integer" 1>&2
    exit 1
fi

NODE_NAME=dstwkr$NAME@$HOST

if [ "$FRESH" = 1 ]; then
    echo "Removing old data folder..."
    rm -rf $DIR/data/$NAME
    mkdir -p $DIR/data/$NAME/ring
fi

exec erl -pa ebin \
    -boot start_sasl \
    -setcookie $COOKIE \
    -name $NODE_NAME \
    -env ERL_LIBS $LIBS_DIR \
    -config $CONFIG \
    -eval "application:ensure_all_started(dstwkr)" \
    -riak_core platform_data_dir "\"data/$NAME\"" \
    -riak_core ring_state_dir "\"data/$NAME/ring\"" \
    -riak_core handoff_port $(($BASE_HANDOFF_PORT + $NAME)) \
    -s sync go
