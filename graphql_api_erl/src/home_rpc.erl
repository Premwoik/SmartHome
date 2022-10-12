-module(home_rpc).

-export([call/3]).

-define(Node, 'smart_home@localhost').

call(Mod, Fun, Args) ->
    rpc:call(?Node, Mod, Fun, Args).
