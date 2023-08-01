-module(example_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one},
    ChildSpecs = [
        surreal:child_spec({"surrealdb://root:root@localhost:8000/test/test", db_conn, #{}})
    ],
    
    {ok, {SupFlags, ChildSpecs}}.
