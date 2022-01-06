#!/bin/sh
export LC_ALL=en_GB.UTF-8
export LC_CTYPE=en_GB.UTF-8

iex --name sh@192.168.2.100 --cookie COOKIE -S mix phx.server

