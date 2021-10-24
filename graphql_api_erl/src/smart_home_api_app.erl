%%%-------------------------------------------------------------------
%% @doc graph_ql_api public API
%% @end
%% rebar3 shell --sname smart_home_api@localhost
%%%-------------------------------------------------------------------

-module(smart_home_api_app).

-behaviour(application).

-export([start/2, stop/1, query/2]).

start(_Type, _Args) ->
    ok = start_cowboy_server(),
    ok = load_schema(),
    smart_home_api_sup:start_link().

stop(_State) ->
	ok = cowboy:stop_listener(http).

%% internal functions
%%
query(OpName, Map) ->
    {ok, Ast} = graphql:parse(Map),
    {ok, #{ast := AST2 }} = graphql:type_check(Ast),
    ok = graphql:validate(AST2),
    Ctx = #{params => #{}, operation_name => OpName},
    graphql:execute(Ctx, AST2).

start_cowboy_server() ->
    Dispatch =
        cowboy_router:compile(
          [{'_',
            [{"/assets/[...]", cowboy_static,
              {priv_dir, smart_home_api, "site/assets"}},
             {"/", graphql_handler,
              {priv_file, smart_home_api, "site/index.html"}}
            ]}]),
    {ok, Port} = application:get_env(smart_home_api, http_port),
    error_logger:info_msg("Starting HTTP listener on port ~p", [Port]),
    cowboy:start_clear(smart_home_api_http,
                      [{port, Port}],
                      #{env => #{dispatch => Dispatch},
                        stream_handlers => [cowboy_compress_h, cowboy_stream_h],
                        max_request_line_length => 65536,
                        max_header_value_length => 16384
                       }
                      ),
    ok.

load_schema() ->
    {ok, SchemaFile} = application:get_env(smart_home_api, schema_file),
    PrivDir = code:priv_dir(smart_home_api),
    {ok, SchemaData} = file:read_file(
                         filename:join(PrivDir, SchemaFile)),
    Mapping = mapping_rules(),
    ok = graphql:load_schema(Mapping, SchemaData),
    ok = setup_root(),
    ok = graphql:validate_schema(),
    ok.

mapping_rules() ->
    #{
       objects => #{
         'Port' => port_resource,
         'Query' => query_resource,
         'Mutation' => mutation_resource,
         'default' => def_resource
        }
     }.

setup_root() ->
    Root = {root,
            #{ query => 'Query',
               mutation => 'Mutation'
             }},
    ok = graphql:insert_root(Root),
    ok.
