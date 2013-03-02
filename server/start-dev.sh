#!/bin/sh
erl -pa apps/*/ebin deps/*/ebin -s errchat_server \
    -eval "io:format(\"Point your browser at http://localhost:10100/ to use a simple websocket client~n\")."

