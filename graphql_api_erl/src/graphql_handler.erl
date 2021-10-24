%% tag::exports[]
-module(graphql_handler).

%% Cowboy Handler Interface
-export([init/2]).

%% REST callbacks
-export([
    allowed_methods/2,
    resource_exists/2,
    content_types_provided/2,
    content_types_accepted/2,
    charsets_provided/2
]).

%% Data input/output callbacks
-export([
    from_json/2,
    to_json/2,
    to_html/2
]).
%% end::exports[]

%% -- API ---------------------------------------------------
%% tag::init[]
init(Req, {priv_file, _, _} = PrivFile) ->
    {cowboy_rest,
     Req,
     #{ index_location => PrivFile }}.
%% end::init[]

%% tag::allowed_methods[]
allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>], Req, State}.
%% end::allowed_methods[]

%% tag::content_types_accepted[]
content_types_accepted(Req, State) ->
    {[
        {{<<"application">>, <<"json">>, []}, from_json}
    ], Req, State}.
%% end::content_types_accepted[]

%% tag::content_types_provided[]
content_types_provided(Req, State) ->
    {[
        {{<<"application">>, <<"json">>, []}, to_json},
        {{<<"text">>, <<"html">>, []}, to_html}
    ], Req, State}.
%% end::content_types_provided[]

%% tag::charsets_provided[]
charsets_provided(Req, State) ->
    {[<<"utf-8">>], Req, State}.
%% end::charsets_provided[]

%% tag::resource_exists[]
resource_exists(#{ method := <<"GET">> } = Req, State) ->
    {true, Req, State};
resource_exists(#{ method := <<"POST">> } = Req, State) ->
    {false, Req, State}.
%% end::resource_exists[]

%% tag::to_html[]
to_html(Req, #{ index_location :=
                    {priv_file, App, FileLocation}} = State) ->
    Filename = filename:join(code:priv_dir(App), FileLocation),
    {ok, Data} = file:read_file(Filename),
    {Data, Req, State}.
%% end::to_html[]

%% tag::json_processing[]
json_request(Req, State) ->
    case gather(Req) of
        {error, Reason} ->
            err(400, Reason, Req, State);
        {ok, Req2, Decoded} ->
            run_request(Decoded, Req2, State)
    end.

from_json(Req, State) -> json_request(Req, State).
to_json(Req, State) -> json_request(Req, State).
%% end::json_processing[]

%% -- INTERNAL FUNCTIONS ---------------------------------------

%% tag::run_request[]
run_request(#{ document := undefined }, Req, State) ->
    err(400, no_query_supplied, Req, State);
run_request(#{ document := Doc} = ReqCtx, Req, State) ->
    case graphql:parse(Doc) of
        {ok, AST} ->
            run_preprocess(ReqCtx#{ document := AST }, Req, State);
        {error, Reason} ->
            err(400, Reason, Req, State)
    end.
%% end::run_request[]

%% tag::run_preprocess[]
run_preprocess(#{ document := AST } = ReqCtx, Req, State) ->
    try
        {ok, #{
           fun_env := FunEnv,
           ast := AST2 }} = graphql:type_check(AST), % <2>
        ok = graphql:validate(AST2), % <3>
        run_execute(ReqCtx#{ document := AST2, fun_env => FunEnv }, Req, State)
    catch
        throw:Err ->
            err(400, Err, Req, State)
    end.
%% end::run_preprocess[]

%% tag::run_execute[]
run_execute(#{ document := AST,
               fun_env := FunEnv,
               vars := Vars,
               operation_name := OpName }, Req, State) ->
    Coerced = graphql:type_check_params(FunEnv, OpName, Vars), % <1>
    Ctx = #{
      params => Coerced,
      operation_name => OpName },
    Response = graphql:execute(Ctx, AST), % <2>
    ResponseBody = response:term_to_json(Response), % <3>
    Req2 = cowboy_req:set_resp_body(ResponseBody, Req), % <4>
    Reply = cowboy_req:reply(200, Req2),
    {stop, Reply, State}.
%% end::run_execute[]

%% tag::gather[]
gather(Req) ->
    {ok, Body, Req2} = cowboy_req:read_body(Req),
    Bindings = cowboy_req:bindings(Req2),
    try jsx:decode(Body, [return_maps]) of
        JSON ->
            gather(Req2, JSON, Bindings)
    catch
        error:badarg ->
            {error, invalid_json_body}
    end.

gather(Req, Body, Params) ->
    QueryDocument = document([Params, Body]),
    case variables([Params, Body]) of
        {ok, Vars} ->
            Operation = operation_name([Params, Body]),
            {ok, Req, #{ document => QueryDocument,
                         vars => Vars,
                         operation_name => Operation}};
        {error, Reason} ->
            {error, Reason}
    end.
%% end::gather[]

%% tag::document[]
document([#{ <<"query">> := Q }|_]) -> Q;
document([_|Next]) -> document(Next);
document([]) -> undefined.
%% end::document[]

%% tag::variables[]
variables([#{ <<"variables">> := Vars} | _]) ->
  if
      is_binary(Vars) ->
          try jsx:decode(Vars, [return_maps]) of
              null -> {ok, #{}};
              JSON when is_map(JSON) -> {ok, JSON};
              _ -> {error, invalid_json}
          catch
              error:badarg ->
                  {error, invalid_json}
          end;
      is_map(Vars) ->
          {ok, Vars};
      Vars == null ->
          {ok, #{}}
  end;
variables([_ | Next]) ->
    variables(Next);
variables([]) ->
    {ok, #{}}.
%% end::variables[]

%% tag::operation_name[]
operation_name([#{ <<"operationName">> := OpName } | _]) ->
    OpName;
operation_name([_ | Next]) ->
    operation_name(Next);
operation_name([]) ->
    undefined.
%% tag::operation_name[]


%% tag::errors[]
err(Code, Msg, Req, State) ->
    Formatted = iolist_to_binary(io_lib:format("~p", [Msg])),
    Err = #{ type => error,
             message => Formatted },
    Body = jsx:encode(#{ errors => [Err] }),
    Req2 = cowboy_req:set_resp_body(Body, Req),
    Reply = cowboy_req:reply(Code, Req2),
    {stop, Reply, State}.
%% end::errors[]
