%%%-------------------------------------------------------------------------
%%% @copyright (C) 2023, meppu
%%% @doc SurrealDB driver for BEAM ecosystem.
%%% @author meppu
%%% @end
%%%-------------------------------------------------------------------------
-module(surreal).

-export([child_spec/1, start_link/2, start_link/3, ping/1, close/1]).
-export([signin/3, signin/2, signup/2, use/3, authenticate/2, invalidate/1]).
-export([query/2, query/3, select/2, create/3, insert/3, update/3, merge/3, patch/3, delete/2]).
-export([set/3, unset/2]).

-export_type([surreal_opts/0]).

%%%==========================================================================
%%%
%%%   Public types
%%%
%%%==========================================================================

-type surreal_opts() :: #{
    signin => signin_opts(),
    use => use_opts()
}.

-type signin_opts() ::
    boolean()
    | (Vars :: map()).
%% This is utilised to define options for the sign-in process.
%% It offers three possible options:
%%
%% <dl>
%%   <dt>`true'</dt>
%%   <dd>This is the default option, and it indicates that the sign-in process is enabled and should be used with the given URI.</dd>
%%
%%   <dt>`false'</dt>
%%   <dd>This option stands for disabling the sign-in process entirely.</dd>
%%
%%   <dt>`(Vars :: map())'</dt>
%%   <dd>This option provides flexibility for custom sign-in options.</dd>
%% </dl>

-type use_opts() :: boolean().
%% This type represents a boolean value.
%% It is used to determine whether specific database and namespace settings should be used or not.
%% Default is `true'.

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
%% @doc Child specification for SurrealDB Erlang.
%% @end
%%-------------------------------------------------------------------------
-spec child_spec({Uri, ConnName, Opts}) -> supervisor:child_spec() when
    Uri :: nonempty_string(), ConnName :: atom(), Opts :: surreal_opts().
child_spec({Uri, ConnName, Opts}) ->
    #{
        id => ConnName,
        start => {?MODULE, start_link, [Uri, ConnName, Opts]},
        restart => transient
    }.

%%-------------------------------------------------------------------------
%% @doc Connects to a local or remote database endpoint with additional options.
%% Essentially the same as {@link surreal:start_link/2} but with an extra argument.
%%
%% - `Opts' allows you to provide custom options.
%%
%% ```
%1> Uri = "surrealdb://root:root@localhost:8000/surrealdb/docs".
%%  % "surrealdb://root:root@localhost:8000/surrealdb/docs"
%2> {ok, Pid} = surreal:start_link(Uri, database, #{use => false}).
%%  % {ok,<0.359.0>}
%% '''
%% @end
%%-------------------------------------------------------------------------
-spec start_link(Uri, ConnName, Opts) -> gen_server:start_ret() when
    Uri :: nonempty_string(), ConnName :: atom(), Opts :: surreal_opts().
start_link(Uri, ConnName, Opts) ->
    {ok,
        Config = #{
            username := Username,
            password := Password,
            namespace := Namespace,
            database := Database
        }} = surreal_config:parse(Uri),

    case surreal_connection:start_link(Config, ConnName) of
        {ok, Pid} ->
            {ok, _} =
                case maps:get(signin, Opts, true) of
                    true ->
                        signin(Pid, Username, Password);
                    false ->
                        {ok, null};
                    Vars when is_map(Vars) ->
                        {ok, Token} = signin(Pid, Vars),
                        authenticate(Pid, Token)
                end,

            {ok, _} =
                case maps:get(use, Opts, true) of
                    true ->
                        use(Pid, Namespace, Database);
                    false ->
                        {ok, null}
                end,

            {ok, Pid};
        Other ->
            Other
    end.

%%-------------------------------------------------------------------------
%% @doc Establishes a connection to a local or remote database endpoint.
%%
%% - `Uri' must be a valid SurrealDB URI. For more information, visit {@link surreal_config}.
%%
%% - `ConnName' allows you to set a name for the connection, so you can use the given name instead of pid while using SurrealDB.
%%
%% ```
%1> Uri = "surrealdb://root:root@localhost:8000/surrealdb/docs".
%%  % "surrealdb://root:root@localhost:8000/surrealdb/docs"
%2> {ok, Pid} = surreal:start_link(Uri, database).
%%  % {ok,<0.359.0>}
%% '''
%% @end
%%-------------------------------------------------------------------------
-spec start_link(Uri :: nonempty_string(), ConnName :: atom()) -> gen_server:start_ret().
start_link(Uri, ConnName) ->
    start_link(Uri, ConnName, #{}).

%%-------------------------------------------------------------------------
%% @since 2.1.0
%% @doc Ping SurrealDB instance.
%%
%% ```
%1> surreal:ping(Pid).
%%  % {ok,null}
%% '''
%% @end
%%-------------------------------------------------------------------------
-spec ping(surreal_pid()) -> surreal_result:result().
ping(Pid) ->
    send_handle_message(Pid, <<"ping">>, null).

%%-------------------------------------------------------------------------
%% @doc Closes the persistent connection to the database.
%%
%% ```
%1> surreal:close(Pid).
%%  % ok
%% '''
%% @end
%%-------------------------------------------------------------------------
-spec close(surreal_pid()) -> ok.
close(Pid) ->
    surreal_connection:close(Pid).

%%-------------------------------------------------------------------------
%% @doc Signs in to the database with a username and password.
%%
%% ```
%1> surreal:signin(Pid, "root", "root").
%%  % {ok,null}
%% '''
%% @end
%%-------------------------------------------------------------------------
-spec signin(surreal_pid(), Username :: iodata(), Password :: iodata()) -> surreal_result:result().
signin(Pid, Username, Password) ->
    Params = [
        #{
            <<"user">> => unicode:characters_to_binary(Username),
            <<"pass">> => unicode:characters_to_binary(Password)
        }
    ],
    send_handle_message(Pid, <<"signin">>, Params).

%%-------------------------------------------------------------------------
%% @doc Signs in to a root, namespace, database or scope user.
%%
%% For more information about `Vars', view
%% <a href="https://surrealdb.com/docs/integration/websocket/text#signin" target="_blank">SurrealDB documentation</a>
%% and the
%% <a href="https://surrealdb.com/docs/integration/sdks/nodejs#signin" target="_blank">JavaScript SDK</a>.
%%
%% ```
%1> Vars = #{<<"DB">> => <<"test">>,<<"NS">> => <<"test">>,
%1>  <<"SC">> => <<"user">>,
%1>  <<"email">> => <<"info@surrealdb.com">>,
%1>  <<"pass">> => <<"123456">>}.
%%  % #{...}
%2> surreal:signin(Pid, Vars).
%%  % {ok, <<"eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc"...>>}
%% '''
%% @end
%%-------------------------------------------------------------------------
-spec signin(surreal_pid(), Vars :: map()) -> surreal_result:result().
signin(Pid, Vars) ->
    Params = [Vars],
    send_handle_message(Pid, <<"signin">>, Params).

%%-------------------------------------------------------------------------
%% @doc Signs up a user to a specific authentication scope.
%%
%% For more information about `Vars', view
%% <a href="https://surrealdb.com/docs/integration/websocket/text#signup" target="_blank">SurrealDB documentation</a>
%% and the
%% <a href="https://surrealdb.com/docs/integration/sdks/nodejs#signup" target="_blank">JavaScript SDK</a>.
%%
%% ```
%1> Vars = #{<<"DB">> => <<"test">>,<<"NS">> => <<"test">>,
%1>  <<"SC">> => <<"user">>,
%1>  <<"email">> => <<"info@surrealdb.com">>,
%1>  <<"pass">> => <<"123456">>}.
%%  % #{...}
%2> surreal:signup(Pid, Vars).
%%  % {ok, <<"eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc"...>>}
%% '''
%% @end
%%-------------------------------------------------------------------------
-spec signup(surreal_pid(), Vars :: map()) -> surreal_result:result().
signup(Pid, Vars) ->
    Params = [Vars],
    send_handle_message(Pid, <<"signup">>, Params).

%%-------------------------------------------------------------------------
%% @doc Switches to a specific namespace and database.
%%
%% ```
%1> surreal:use(Pid, "test", "test").
%%  % {ok,null}
%% '''
%% @end
%%-------------------------------------------------------------------------
-spec use(surreal_pid(), Namespace :: iodata(), Database :: iodata()) -> surreal_result:result().
use(Pid, Namespace, Database) ->
    Params = [unicode:characters_to_binary(Namespace), unicode:characters_to_binary(Database)],
    send_handle_message(Pid, <<"use">>, Params).

%%-------------------------------------------------------------------------
%% @doc Authenticates the current connection with a JWT token.
%%
%% ```
%1> Token = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc...".
%%  % "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc..."
%2> surreal:authenticate(Pid, Token).
%%  % {ok,null}
%% '''
%% @end
%%-------------------------------------------------------------------------
-spec authenticate(surreal_pid(), Token :: iodata()) -> surreal_result:result().
authenticate(Pid, Token) ->
    Params = [unicode:characters_to_binary(Token)],
    send_handle_message(Pid, <<"authenticate">>, Params).

%%-------------------------------------------------------------------------
%% @doc Invalidates the authentication for the current connection.
%%
%% ```
%1> surreal:invalidate(Pid).
%%  % {ok,null}
%% '''
%% @end
%%-------------------------------------------------------------------------
-spec invalidate(surreal_pid()) -> surreal_result:result().
invalidate(Pid) ->
    send_handle_message(Pid, <<"invalidate">>, null).

%%-------------------------------------------------------------------------
%% @since 2.1.0
%% @doc Executes a set of SurrealQL statements against the database.
%%
%% ```
%1> [{ok, Result}] = surreal:query(Pid, "SELECT * FROM authorised").
%%  % {ok,[#{<<"id">> => <<"authorised:3n4mn5wsq823i7pgv9un">>,
%%  %        <<"user">> => <<"A">>},
%%  %      #{<<"id">> => <<"authorised:raedq65doxhpuc6t3meo">>,
%%  %        <<"user">> => <<"B">>}]}
%% '''
%% @end
%%-------------------------------------------------------------------------
-spec query(surreal_pid(), Query :: iodata()) -> surreal_result:result().
query(Pid, Query) ->
    query(Pid, Query, #{}).

%%-------------------------------------------------------------------------
%% @doc Executes a set of SurrealQL statements against the database with variables.
%%
%% This function is similar to {@link query/2}, but allows you to provide a map of variables
%% that can be used in the query.
%%
%% This can be useful for dynamic queries where you need to parametrise certain parts
%% of the query.
%%
%% ```
%1> [{ok, Result}] = surreal:query(Pid, "SELECT * FROM authorised WHERE user = $user", #{<<"user">> => <<"A">>}).
%%  % [{ok,[#{<<"id">> => <<"authorised:3n4mn5wsq823i7pgv9un">>,
%%  %        <<"user">> => <<"A">>}]}]
%% '''
%% @end
%%-------------------------------------------------------------------------
-spec query(surreal_pid(), Query :: iodata(), Variables :: map()) -> surreal_result:result().
query(Pid, Query, Variables) ->
    Params = [unicode:characters_to_binary(Query), Variables],
    send_handle_query_message(Pid, <<"query">>, Params).

%%-------------------------------------------------------------------------
%% @doc Retrieves all records in a table or a specific record.
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
-spec select(surreal_pid(), Thing :: iodata()) -> surreal_result:result().
select(Pid, Thing) ->
    Params = [unicode:characters_to_binary(Thing)],
    send_handle_message(Pid, <<"select">>, Params).

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
-spec create(surreal_pid(), Thing :: iodata(), Data :: map() | null) -> surreal_result:result().
create(Pid, Thing, Data) ->
    Params = [unicode:characters_to_binary(Thing), Data],
    send_handle_message(Pid, <<"create">>, Params).

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
    Thing :: iodata(),
    Data :: map() | list(map()).
insert(Pid, Thing, Data) ->
    Params = [unicode:characters_to_binary(Thing), Data],
    send_handle_message(Pid, <<"insert">>, Params).

%%-------------------------------------------------------------------------
%% @doc Updates all records in a table, or a specific record, in the database.
%% This function replaces the current document/record data with the specified data.
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
-spec update(surreal_pid(), Thing :: iodata(), NewData :: map() | null) -> surreal_result:result().
update(Pid, Thing, Data) ->
    Params = [unicode:characters_to_binary(Thing), Data],
    send_handle_message(Pid, <<"update">>, Params).

%%-------------------------------------------------------------------------
%% @doc Modifies all records in a table, or a specific record, in the database.
%% This function merges the current document/record data with the specified data.
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
-spec merge(surreal_pid(), Thing :: iodata(), Data :: map() | null) -> surreal_result:result().
merge(Pid, Thing, Data) ->
    Params = [unicode:characters_to_binary(Thing), Data],
    send_handle_message(Pid, <<"merge">>, Params).

%%-------------------------------------------------------------------------
%% @doc Applies JSON Patch changes to all records, or a specific record, in the database.
%% This function patches the current document/record data with the specified JSON Patch data.
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
    Thing :: iodata(),
    JSONPatch :: list(surreal_patch:patch()) | null.
patch(Pid, Thing, JSONPatch) ->
    Params = [unicode:characters_to_binary(Thing), surreal_patch:convert(JSONPatch)],
    send_handle_message(Pid, <<"patch">>, Params).

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
-spec delete(surreal_pid(), Thing :: iodata()) -> surreal_result:result().
delete(Pid, Thing) ->
    Params = [unicode:characters_to_binary(Thing)],
    send_handle_message(Pid, <<"delete">>, Params).

%%-------------------------------------------------------------------------
%% @doc Creates a variable that can be used throughout the database session.
%%
%% ```
%1> surreal:set(Pid, "PI", 3.14).
%%  % {ok,null}
%% '''
%% @end
%%-------------------------------------------------------------------------
-spec set(surreal_pid(), Variable :: iodata(), Value :: term()) -> surreal_result:result().
set(Pid, Variable, Value) ->
    Params = [unicode:characters_to_binary(Variable), Value],
    send_handle_message(Pid, <<"let">>, Params).

%%-------------------------------------------------------------------------
%% @doc Removes a variable from the current socket connection.
%%
%% ```
%1> surreal:unset(Pid, "PI").
%%  % {ok,null}
%% '''
%% @end
%%-------------------------------------------------------------------------
-spec unset(surreal_pid(), Variable :: iodata()) -> surreal_result:result().
unset(Pid, Variable) ->
    Params = [unicode:characters_to_binary(Variable)],
    send_handle_message(Pid, <<"unset">>, Params).

%%%==========================================================================
%%%
%%%   Private functions
%%%
%%%==========================================================================

%% @private
send_handle_message(Pid, Method, Params) ->
    case surreal_connection:send_message(Pid, Method, Params) of
        {ok, Response} ->
            surreal_result:get_method_result(Response);
        Other ->
            Other
    end.

%% @private
send_handle_query_message(Pid, Method, Params) ->
    case send_handle_message(Pid, Method, Params) of
        {ok, Response} ->
            surreal_result:get_query_result(Response);
        Other ->
            Other
    end.
