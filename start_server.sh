#!/bin/sh
echo "Starting errchat server"
cd server
rebar compile
sh start-dev.sh