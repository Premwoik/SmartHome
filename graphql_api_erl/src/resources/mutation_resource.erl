-module(mutation_resource).

-export([execute/4]).

execute(_Ctx, _Obj, <<"updateState">>, #{ <<"id">> := Id, <<"state">> := State}) ->
    {ok, Port} = home_rpc:call('Elixir.DB.Proc.PortListProc', update_state , [Id, State]),
    {ok, Port}.
