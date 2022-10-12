#!/bin/sh
export LC_ALL=en_GB.UTF-8
export LC_CTYPE=en_GB.UTF-8

if ! [[ $1 = "prod" ]] ; then
export TARGET=mac
fi

iex --name sh@192.168.2.112 --cookie COOKIE -S mix phx.server

