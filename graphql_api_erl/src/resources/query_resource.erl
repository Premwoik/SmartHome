-module(query_resource).

-export([execute/4]).

execute(_Ctx, _Obj, <<"ports">>, _Args) ->
    {ok, Ports} = home_rpc:call('Elixir.DB.Proc.PortListProc', list_all, []),
    Ports2 = lists:map(fun(P) -> {ok, P} end, Ports),
    {ok, Ports2};
execute(_Ctx, _Obj, <<"port">>, #{<<"id">> := Id}) ->
    case home_rpc:call('Elixir.DB.Proc.PortListProc', get, [Id]) of
        {ok, Port} -> 
            {ok, Port};
        _ -> 
            {error, <<"Not found">>}
    end.

