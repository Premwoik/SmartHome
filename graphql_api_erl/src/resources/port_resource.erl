-module(port_resource).

-export([execute/4]).

execute(_Ctx, #{'__struct__' := 'Elixir.DB.Data.Port', id := Id}, <<"id">> , _Args) ->
    {ok, Id};
execute(_Ctx, #{'__struct__' := 'Elixir.DB.Data.Port', name := Name}, <<"name">> , _Args) ->
    {ok, Name};
execute(_Ctx, #{'__struct__' := 'Elixir.DB.Data.Port', state := State}, <<"state">> , _Args) ->
    erlang:display(State),
    {ok, State}.
