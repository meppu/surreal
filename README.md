<div align="center">

![banner](.github/assets/banner.webp)

# SurrealDB Erlang

Erlang driver for SurrealDB.

</div>

> ⚠️ You are currently viewing version 1 ⚠️

Package name named as [surreal on hex](https://hex.pm/packages/surreal). Because an Erlang implementation can be used in other languages that runs on BEAM virtual machine, such as Elixir and Gleam.

This library tries to be compatible with the [official implementation.](https://github.com/surrealdb/surrealdb.js)

## Installation

Add `surreal` to your list of dependencies in `rebar.config` file:

```erlang
{deps, [{surreal, "1.1.0"}]}.
```

## Usage

### Simple Connection

```erlang
1> {ok, Pid} = surreal:start_link("ws://localhost:8000").
% {ok, <pid>}
2> surreal:signin(Pid, "root", "root").
% {ok, null}
3> surreal:use(Pid, "test", "test").
% {ok, null}
4> surreal:create(Pid, "example:bob",
4>  #{<<"name">> => <<"bob">>}).
% {ok, #{<<"id">> => <<"example:bob">>, <<"name">> => <<"bob">>}},
5> surreal:query(Pid, "SELECT * FROM type::table($tb);",
5>    #{<<"tb">> => <<"test">>}).
% [{ok, [#{<<"id">> => <<"example:bob">>, <<"name">> => <<"bob">>}]}]
```

### Connect with Config

```erlang
1> Config = [
1>   % link the process
1>   link,
1>   % register local name
1>   {name, example_client},
1>   % host and port
1>   {host, "localhost"},
1>   {port, 8000},
1>   % database, namespace to use
1>   {use, {"test", "test"}},
1>   % user, pass auth
1>   {signin, {"root", "root"}}
1> ].
% ...
2> {ok, Pid} = surreal_config:load(Config).
% ...
3> surreal:create(example_client, "example:bob",
3>  #{<<"name">> => <<"bob">>}).
% {ok, #{<<"id">> => <<"example:bob">>, <<"name">> => <<"bob">>}},
```

## Documentation

Documentation is available at [HexDocs](https://hexdocs.pm/surreal).

## Contributing

You can always report bugs and request features via [GitHub Issues](/issues).

For pull requests, make sure your code is well-formatted and at least can explain itself.

## License

SurrealDB Erlang is licensed under the MIT License.
