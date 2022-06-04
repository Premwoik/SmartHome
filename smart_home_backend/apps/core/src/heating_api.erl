-module(heating_api).

-export([write_pin/3, run_circut/2, get_temps/1, get_config/1, set_config/2,
         is_registered_observer/2, register_observer/2, unregister_observer/2]).

-include_lib("../../../deps/basement_core/src/heating.hrl").

-type device() :: 'Elixir.DB.Data.Device':t().
-type circut_map() ::
    #{name := atom(),
      port_id => integer(),
      max_temp := float(),
      min_temp := float(),
      planned_runs := planned_run(),
      running_duration := calendar:time(),
      break_duration := calendar:time(),
      atom() => any()}.
-type config() ::
    #{boiler_min_temp := float(),
      temp_read_interval := calendar:time(),
      circuts := [circut_map()],
      atom() => any()}.

-define(MOD, heating_server).
-define(HARDWARE, hardware_api).

%% Api

-spec write_pin(device(), integer(), true | false) -> ok | node_issue.
write_pin(#{ip := Node}, Pin, State) ->
    State2 =
        if State ->
               low;
           true ->
               high
        end,
    hcall(Node, ?FUNCTION_NAME, [Pin, State2]).

-spec run_circut(device(), atom() | integer()) -> ok | node_issue.
run_circut(#{ip := Node}, CircutID) ->
    call(Node, ?FUNCTION_NAME, [CircutID]).

-spec get_temps(device()) -> ok | node_issue.
get_temps(#{ip := Node}) ->
    call(Node, ?FUNCTION_NAME, []).

-spec get_config(device()) -> {ok, config()} | node_issue.
get_config(#{ip := Node}) ->
    case call(Node, ?FUNCTION_NAME, []) of
        {ok, Config} ->
            {ok, state_to_map(Config)};
        _ ->
            node_issue
    end.

-spec set_config(device(), config()) -> ok.
set_config(#{ip := Node}, Config) ->
    State = map_to_state(Config),
    ok = rpc:call(Node, heating_server, set_config, [State]),
    ok.

-spec register_observer(device(), pid()) -> ok.
register_observer(#{ip := Node}, Pid) ->
    call(Node, ?FUNCTION_NAME, [Pid]).

-spec unregister_observer(device(), pid()) -> ok.
unregister_observer(#{ip := Node}, Pid) ->
    call(Node, ?FUNCTION_NAME, [Pid]).

-spec is_registered_observer(device(), pid()) -> boolean().
is_registered_observer(#{ip := Node}, Pid) ->
    case call(Node, ?FUNCTION_NAME, [Pid]) of
        {ok, Status} ->
            Status;
        node_issue ->
            false
    end.

%% Internal
-spec call(binary(), atom(), list()) -> any().
call(Node, Function, Args) ->
    call(Node, ?MOD, Function, Args).

-spec hcall(binary(), atom(), list()) -> any().
hcall(Node, Function, Args) ->
    call(Node, ?HARDWARE, Function, Args).

-spec call(binary(), atom(), atom(), list()) -> any().
call(Node, Mod, Function, Args) ->
    try
        case rpc:call(binary_to_atom(Node), Mod, Function, Args) of
            {badrpc, _} ->
                node_issue;
            Res ->
                Res
        end
    catch
        _:_ ->
            node_issue
    end.

-spec state_to_map(#state{}) -> map().
state_to_map(State) ->
    Circuts = lists:map(fun record_to_map/1, State#state.circuts),
    Pomp = record_to_map(State#state.pomp),
    StateMap = record_to_map(State),
    StateMap#{circuts => Circuts, pomp => Pomp}.

-spec map_to_state(map()) -> #state{}.
map_to_state(StateMap) ->
    State = map_to_record(StateMap, state),
    Circuts = lists:map(fun(M) -> map_to_record(M, circut) end, State#state.circuts),
    Pomp = map_to_record(maps:get(pomp, State), cwu_pomp),
    State#state{circuts = Circuts, pomp = Pomp}.

map_to_record(Map, RecordType) ->
    RecordFields = record_fields(RecordType),
    RecordValues = [maps:get(RecordField, Map, undefined) || RecordField <- RecordFields],
    AlmostRecord = [RecordType | RecordValues],
    list_to_tuple(AlmostRecord).

record_to_map(Record) ->
    [RecordType | Values] = tuple_to_list(Record),
    RecordFields = record_fields(RecordType),
    maps:from_list(
        lists:zip(RecordFields, Values)).

record_fields(state) ->
    record_info(fields, state);
record_fields(circut) ->
    record_info(fields, circut);
record_fields(cwu_pomp) ->
    record_info(fields, cwu_pomp);
record_fields(_) ->
    [].
