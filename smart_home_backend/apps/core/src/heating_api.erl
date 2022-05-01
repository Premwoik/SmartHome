-module(heating_api).

-export([run_circut/2, get_temps/1, get_config/1, set_config/2, register_observer/2,
         unregister_observer/2]).

-export([config_mock/0]).

-include_lib("../../../deps/basement_core/src/heating.hrl").

-type device() :: 'Elixir.DB.Data.Device':t().
-type circut() ::
    #{name := atom(),
      max_temp := float(),
      min_temp := float(),
      planned_runs := planned_run(),
      running_duration := calendar:time(),
      break_duration := calendar:time()}.
-type config() ::
    #{boiler_min_temp := float(),
      temp_read_interval := calendar:time(),
      circuts := [circut()]}.

-define(MOD, heating_server).

%% Debug

config_mock() ->
    C1 = #{name => high,
           max_temp => 30.0,
           min_temp => 20.0,
           planned_runs => [{[mon], {6, 20, 0}, {1, 0, 0}}, {null, {8, 0, 0}, {1, 0, 0}}],
           running_duration => {0, 10, 0},
           break_duration => {0, 5, 0}},
    C2 = #{name => low,
           max_temp => 30.0,
           min_temp => 20.0,
           planned_runs => [],
           running_duration => {0, 10, 0},
           break_duration => {0, 5, 0}},
    #{boiler_min_temp => 40.0,
      temp_read_interval => {0, 0, 30},
      circuts => [C1, C2]}.

%% Api

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

%% Internal
-spec call(binary(), atom(), list()) -> any().
call(Node, Function, Args) ->
    try
        rpc:call(binary_to_atom(Node), ?MOD, Function, Args)
    catch
        _:_ ->
            node_issue
    end.

-spec state_to_map(#state{}) -> map().
state_to_map(State) ->
    Circuts = lists:map(fun record_to_map/1, State#state.circuts),
    StateMap = record_to_map(State),
    StateMap#{circuts => Circuts}.

-spec map_to_state(map()) -> #state{}.
map_to_state(StateMap) ->
    State = map_to_record(StateMap, state),
    Circuts = lists:map(fun(M) -> map_to_record(M, circut) end, State#state.circuts),
    State#state{circuts = Circuts}.

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
record_fields(_) ->
    [].
