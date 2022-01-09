#!/bin/sh
export LC_ALL=en_GB.UTF-8
export LC_CTYPE=en_GB.UTF-8
export RELEASE_DISTRIBUTION=name
export RELEASE_NODE=sh@192.168.2.100
export PHX_SERVER=true

export $(grep -v '^#' .priv_env | xargs)

_build/prod/rel/ui/bin/ui start_iex

#cd /home/pi/SmartHome/smart_home_backend
#iex --name sh@192.168.2.100 --cookie COOKIE -S mix phx.server

