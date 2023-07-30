%%%-------------------------------------------------------------------------
%%% @copyright (C) 2023, meppu
%%% @doc Erlang driver for SurrealDB.
%%% @author meppu
%%% @end
%%%-------------------------------------------------------------------------
-module(surreal).

-export([
    start_link/2,
    signin/3,
    use/3,
    authenticate/2,
    invalidate/1,
    query/3,
    select/2,
    create/3,
    insert/3,
    update/3,
    merge/3,
    patch/3,
    delete/2,
    set/3,
    unset/2
]).

%%%==========================================================================
%%%
%%%   Private types
%%%
%%%==========================================================================

-type surreal_pid() :: gen_server:server_ref().

%%%==========================================================================
%%%
%%%   Public functions
%%%
%%%==========================================================================

%%-------------------------------------------------------------------------
%% @doc Connects to a local or remote database endpoint.
%%
%% `Url' must be a valid SurrealDB URI. Visit {@link surreal_config. surreal_config module} for more information.
%%
%% `ConnName' allows you to set a name for connection so you can use given name instead of pid while using SurrealDB.
%%
%% ```
%1> {ok, Pid} = surreal:start_link("surrealdb://root:root@localhost:8000/google/domains", database).
%%  % {ok,<0.359.0>}
%% '''
%% @end
%%-------------------------------------------------------------------------
-spec start_link(Url :: string(), ConnName :: atom()) -> gen_server:start_ret().
start_link(Url, ConnName) ->
    {ok,
        Config = #{
            username := Username,
            password := Password,
            namespace := Namespace,
            database := Database
        }} = surreal_config:parse(Url),

    case surreal_connection:start_link(Config, ConnName) of
        {ok, Pid} ->
            {ok, _} = signin(Pid, Username, Password),
            {ok, _} = use(Pid, Namespace, Database),

            {ok, Pid};
        Other ->
            Other
    end.

%%-------------------------------------------------------------------------
%% @doc Sign in to the database. This is a necessary step before using the database.
%%
%% ```
%1> surreal:signin(Pid, "root", "root").
%%  % {ok, null}
%% '''
%% @end
%%-------------------------------------------------------------------------
-spec signin(surreal_pid(), Username :: string(), Password :: string()) -> surreal_result:result().
signin(Pid, Username, Password) ->
    Params = [
        #{
            <<"user">> => unicode:characters_to_binary(Username),
            <<"pass">> => unicode:characters_to_binary(Password)
        }
    ],

    {ok, Response} = surreal_connection:send_message(Pid, <<"signin">>, Params),
    surreal_result:get_method_result(Response).

%%-------------------------------------------------------------------------
%% @doc Switch to a specific namespace and database.
%%
%% ```
%1> surreal:use(Pid, "test", "test").
%%  % {ok, null}
%% '''
%% @end
%%-------------------------------------------------------------------------
-spec use(surreal_pid(), Namespace :: string(), Database :: string()) -> surreal_result:result().
use(Pid, Namespace, Database) ->
    Params = [unicode:characters_to_binary(Namespace), unicode:characters_to_binary(Database)],

    {ok, Response} = surreal_connection:send_message(Pid, <<"use">>, Params),
    surreal_result:get_method_result(Response).

%%-------------------------------------------------------------------------
%% @doc Authenticates the current connection with a JWT token.
%%
%% ```
%1> Token = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc...".
%%  % "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc..."
%2> surreal:authenticate(Pid, Token).
%% '''
%% @end
%%-------------------------------------------------------------------------
-spec authenticate(surreal_pid(), Token :: string()) -> surreal_result:result().
authenticate(Pid, Token) ->
    Params = [unicode:characters_to_binary(Token)],

    {ok, Response} = surreal_connection:send_message(Pid, <<"authenticate">>, Params),
    surreal_result:get_method_result(Response).

%%-------------------------------------------------------------------------
%% @doc Invalidates the authentication for the current connection.
%%
%% ```
%1> surreal:invalidate(Pid).
%%  % {ok, null}
%% '''
%% @end
%%-------------------------------------------------------------------------
-spec invalidate(surreal_pid()) -> surreal_result:result().
invalidate(Pid) ->
    {ok, Response} = surreal_connection:send_message(Pid, <<"invalidate">>, null),
    surreal_result:get_method_result(Response).

%%-------------------------------------------------------------------------
%% @doc Runs a set of SurrealQL statements against the database.
%%
%% ```
%1> [{ok, Result}] = surreal:query(Pid, "SELECT * FROM authorised WHERE user = $user", #{<<"user">> => <<"A">>}).
%%  % [{ok,[#{<<"id">> => <<"authorised:3n4mn5wsq823i7pgv9un">>,
%%  %        <<"user">> => <<"A">>}]}]
%% '''
%% @end
%%-------------------------------------------------------------------------
-spec query(surreal_pid(), Query :: string(), Variables :: map()) -> surreal_result:result().
query(Pid, Query, Variables) ->
    Params = [unicode:characters_to_binary(Query), Variables],

    {ok, Response} = surreal_connection:send_message(Pid, <<"query">>, Params),
    {ok, Result} = surreal_result:get_method_result(Response),
    surreal_result:get_query_result(Result).

%%-------------------------------------------------------------------------
%% @doc Selects all records in a table, or a specific record.
%%
%% ```
%1> {ok, Result} = surreal:select(Pid, "authorised").
%%  % {ok,[#{<<"id">> => <<"authorised:3n4mn5wsq823i7pgv9un">>,
%%  %        <<"user">> => <<"A">>},
%%  %      #{<<"id">> => <<"authorised:raedq65doxhpuc6t3meo">>,
%%  %        <<"user">> => <<"B">>}]}
%2> {ok, Result2} = surreal:select(Pid, "users:meppu").
%%  % {ok,#{<<"id">> => <<"users:meppu">>,<<"identify">> => <<"cat">>,
%%  %       <<"name">> => <<"meppu">>}}
%% '''
%% @end
%%-------------------------------------------------------------------------
-spec select(surreal_pid(), Thing :: string()) -> surreal_result:result().
select(Pid, Thing) ->
    Params = [unicode:characters_to_binary(Thing)],

    {ok, Response} = surreal_connection:send_message(Pid, <<"select">>, Params),
    surreal_result:get_method_result(Response).

%%-------------------------------------------------------------------------
%% @doc Creates a record in the database.
%%
%% ```
%1> User = #{<<"name">> => <<"meppu">>, <<"identify">> => <<"cat">>}.
%%  % #{<<"identify">> => <<"cat">>,<<"name">> => <<"meppu">>}
%2> {ok, Created} = surreal:create(Pid, "users:meppu", User).
%%  % {ok,#{<<"id">> => <<"users:meppu">>,<<"identify">> => <<"cat">>,
%%  %       <<"name">> => <<"meppu">>}}
%% '''
%% @end
%%-------------------------------------------------------------------------
-spec create(surreal_pid(), Thing :: string(), Data :: map() | null) -> surreal_result:result().
create(Pid, Thing, Data) ->
    Params = [unicode:characters_to_binary(Thing), Data],

    {ok, Response} = surreal_connection:send_message(Pid, <<"create">>, Params),
    surreal_result:get_method_result(Response).

%%-------------------------------------------------------------------------
%% @since 2.0.0
%% @doc Inserts one or multiple records in the database.
%%
%% ```
%1> AuthorisedUsers = [#{<<"user">> => <<"A">>}, #{<<"user">> => <<"B">>}].
%%  % [#{<<"user">> => <<"A">>},#{<<"user">> => <<"B">>}]
%2> {ok, Created} = surreal:insert(Pid, "authorised", AuthorisedUsers).
%%  % {ok,[#{<<"id">> => <<"authorised:3n4mn5wsq823i7pgv9un">>,
%%  %        <<"user">> => <<"A">>},
%%  %      #{<<"id">> => <<"authorised:raedq65doxhpuc6t3meo">>,
%%  %        <<"user">> => <<"B">>}]}
%% '''
%% @end
%%-------------------------------------------------------------------------
-spec insert(surreal_pid(), Thing, Data) -> surreal_result:result() when
    Thing :: string(),
    Data :: map() | list(map()).
insert(Pid, Thing, Data) ->
    Params = [unicode:characters_to_binary(Thing), Data],

    {ok, Response} = surreal_connection:send_message(Pid, <<"insert">>, Params),
    surreal_result:get_method_result(Response).

%%-------------------------------------------------------------------------
%% @doc Updates all records in a table, or a specific record, in the database.
%% This function replaces the current document / record data with the specified data.
%%
%% ```
%1> NewData = #{<<"name">> => <<"meppu">>, <<"identify">> => <<"human">>}.
%%  % #{<<"identify">> => <<"human">>,<<"name">> => <<"meppu">>}
%2> {ok, Updated} = surreal:update(Pid, "users:meppu", NewData).
%%  % {ok,#{<<"id">> => <<"users:meppu">>,<<"identify">> => <<"human">>,
%%  %       <<"name">> => <<"meppu">>}}
%% '''
%% @end
%%-------------------------------------------------------------------------
-spec update(surreal_pid(), Thing :: string(), NewData :: map() | null) -> surreal_result:result().
update(Pid, Thing, Data) ->
    Params = [unicode:characters_to_binary(Thing), Data],

    {ok, Response} = surreal_connection:send_message(Pid, <<"update">>, Params),
    surreal_result:get_method_result(Response).

%%-------------------------------------------------------------------------
%% @doc Modifies all records in a table, or a specific record, in the database.
%% This function merges the current document / record data with the specified data.
%%
%% ```
%1> MergeData = #{<<"score">> => 10}.
%%  % #{<<"score">> => 10}
%2> {ok, Updated} = surreal:merge(Pid, "users:meppu", MergeData).
%%  % {ok,#{<<"id">> => <<"users:meppu">>,<<"identify">> => <<"human">>,
%%  %       <<"name">> => <<"meppu">>,<<"score">> => 10}}
%% '''
%% @end
%%-------------------------------------------------------------------------
-spec merge(surreal_pid(), Thing :: string(), Data :: map() | null) -> surreal_result:result().
merge(Pid, Thing, Data) ->
    Params = [unicode:characters_to_binary(Thing), Data],

    {ok, Response} = surreal_connection:send_message(Pid, <<"merge">>, Params),
    surreal_result:get_method_result(Response).

%%-------------------------------------------------------------------------
%% @doc Applies JSON Patch changes to all records, or a specific record, in the database.
%% This function patches the current document / record data with the specified JSON Patch data.
%%
%% ```
%1> Patches = [{replace, "/name", <<"tuhana">>}, {remove, "/score"}].
%%  % [{replace,"/name",<<"tuhana">>},{remove,"/score"}]
%2> surreal:patch(database, "users:meppu", Patches).
%%  % {ok,[#{<<"op">> => <<"remove">>,<<"path">> => <<"/score">>,
%%  %        <<"value">> => null},
%%  %      #{<<"op">> => <<"change">>,<<"path">> => <<"/name">>,
%%  %        <<"value">> => <<"@@ -1,5 +1,6 @@\n-meppu\n+tuhana\n">>}]}
%% '''
%% @end
%%-------------------------------------------------------------------------
-spec patch(surreal_pid(), Thing, JSONPatch) -> surreal_result:result() when
    Thing :: string(),
    JSONPatch :: list(surreal_patch:patch()) | null.
patch(Pid, Thing, JSONPatch) ->
    Params = [unicode:characters_to_binary(Thing), surreal_patch:convert(JSONPatch)],

    {ok, Response} = surreal_connection:send_message(Pid, <<"patch">>, Params),
    surreal_result:get_method_result(Response).

%%-------------------------------------------------------------------------
%% @doc Deletes all records in a table, or a specific record, from the database.
%%
%% ```
%1> {ok, Deleted} = surreal:merge(Pid, "users:meppu").
%%  % {ok,#{<<"id">> => <<"users:meppu">>,<<"identify">> => <<"human">>,
%%  %       <<"name">> => <<"meppu">>,<<"score">> => 10}}
%% '''
%% @end
%%-------------------------------------------------------------------------
-spec delete(surreal_pid(), Thing :: string()) -> surreal_result:result().
delete(Pid, Thing) ->
    Params = [unicode:characters_to_binary(Thing)],

    {ok, Response} = surreal_connection:send_message(Pid, <<"delete">>, Params),
    surreal_result:get_method_result(Response).

%%-------------------------------------------------------------------------
%% @doc Specify a variable for the current socket connection.
%%
%% ```
%1> surreal:set(Pid, "PI", 3.14).
%%  % {ok, null}
%% '''
%% @end
%%-------------------------------------------------------------------------
-spec set(surreal_pid(), Variable :: string(), Value :: term()) -> surreal_result:result().
set(Pid, Variable, Value) ->
    Params = [unicode:characters_to_binary(Variable), Value],

    {ok, Response} = surreal_connection:send_message(Pid, <<"let">>, Params),
    surreal_result:get_method_result(Response).

%%-------------------------------------------------------------------------
%% @doc Remove a variable from the current socket connection.
%%
%% ```
%1> surreal:unset(Pid, "PI").
%%  % {ok, null}
%% '''
%% @end
%%-------------------------------------------------------------------------
-spec unset(surreal_pid(), Variable :: string()) -> surreal_result:result().
unset(Pid, Variable) ->
    Params = [unicode:characters_to_binary(Variable)],

    {ok, Response} = surreal_connection:send_message(Pid, <<"unset">>, Params),
    surreal_result:get_method_result(Response).
