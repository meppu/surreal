<div align="center">

<img src="https://raw.githubusercontent.com/meppu/surreal/main/.github/assets/banner.webp" alt="banner" />

<h2>SurrealDB Erlang</h2>
<p>SurrealDB driver for BEAM ecosystem</p>

<strong>⚠️ You are currently viewing version 2 ⚠️</strong>

</div>

SurrealDB Erlang, also referred to as "surreal", is a robust and maintainable SurrealDB driver for BEAM ecosystem.

The library draws inspiration from the official [surrealdb.js](https://github.com/surrealdb/surrealdb.js) implementation.

## Installation

Add `surreal` to your list of dependencies in `rebar.config` file:

```erlang
{deps, [{surreal, "2.0.0"}]}.
```

## Getting Started

### Creating a Connection

You can establish a database connection with `surreal:start_link/2` (or `surreal:start_link/3`).

Check out SurrealDB URI format described in the [documentation](https://hexdocs.pm/surreal/surreal_config.html#module-surrealdb-uri-format).

```erlang
{ok, Pid} = surreal:start_link("surrealdb://root:root@localhost:8000/test/test", my_connection).
```

Alternatively, you can use the specified connection name, `my_connection`, in place of `Pid`, as shown below:

```erlang
{ok, Users} = surreal:select(my_connection, "users").
```

### Example Usage

SurrealDB Erlang offers users a clean API, demonstrated below:

```erlang
1> {ok, User} = surreal:create(Pid, "users:meppu", #{<<"score">> => 10}).
   % {ok,#{<<"id">> => <<"users:meppu">>,<<"score">> => 10}}
2> {ok, NewUser} = surreal:merge(Pid, "users:meppu", #{<<"new">> => <<"key">>}).
   % {ok,#{<<"id">> => <<"users:meppu">>,<<"new">> => <<"key">>,
   %       <<"score">> => 10}}
3> [{ok, QueryResp}] = surreal:query(Pid, "SELECT * FROM users WHERE score = $score", #{<<"score">> => 10}).
   % [{ok,[#{<<"id">> => <<"users:meppu">>,<<"new">> => <<"key">>,
   %         <<"score">> => 10}]}]
4> {ok, RemovedUser} = surreal:delete(Pid, "users:meppu").
   % {ok,#{<<"id">> => <<"users:meppu">>,<<"new">> => <<"key">>,
   %       <<"score">> => 10}}
5> RemovedUser =:= NewUser.
   % true
```

### With Supervisor

The recommended approach to initialise a SurrealDB connection is through a supervisor.

You can use `surreal:child_spec/1` to create a child specification for your supervisor, as shown below:

```erlang
%%% ...

-behaviour(supervisor).

%%% ...

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
    SupFlags = #{
      % ...
    },
    ChildSpecs = [
      surreal:child_spec({"surrealdb://root:root@localhost:8000/test/test", db_conn, #{}})
    ],

    {ok, {SupFlags, ChildSpecs}}.
```

### Additional Examples

See additional examples in [examples/](/examples) folder.

## Documentation

For detailed documentation, please refer to [HexDocs](https://hexdocs.pm/surreal).

## Contributing

Feel free to report bugs and request features through [GitHub Issues](/issues).

If you wish to submit a pull request, ensure that your code is well-formatted and easily comprehensible.

### Note for Contributors

Please send your pull requests to `v2` branch instead of the `main` branch.

This helps us demostrate a stable version on the `main` branch while allowing for ongoing development and improvements on the `v2` branch.

## License

SurrealDB Erlang is licensed under the MIT License.
