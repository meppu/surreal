# Example Erlang Rebar3 Project

This example Rebar3 project demonstrates the usage of the SurrealDB erlang library in Erlang.

## Setup

### SurrealDB

1. Install SurrealDB (if not already installed) by following the official installation instructions: [SurrealDB Installation Guide](https://surrealdb.com/docs/installation).

2. Start SurrealDB server:

   ```bash
   surrealdb start --user root --pass root memory --log debug
   ```

### Project

1. Install Erlang (if not already installed).

2. Install Rebar3 (if not already installed).

3. Clone the repository and navigate to the project directory:

   ```bash
   git clone https://github.com/meppu/surreal.git
   cd ./surreal/examples/erlang_rebar3
   ```

4. Fetch and compile the dependencies:

   ```bash
   rebar3 compile
   ```

5. Make sure to update connection URI in `src/example_sup.erl` for your environment.

## Running the Example

To run the example, start a shell:

```bash
rebar3 shell
```

Now you can play with `example_users` module:

```erlang
1> example_users:add("meppu", 16).
{ok,#{<<"age">> => 16,<<"id">> => <<"users:meppu">>,
      <<"verified">> => false}}
2> example_users:get("meppu").
{ok,#{<<"age">> => 16,<<"id">> => <<"users:meppu">>,
      <<"verified">> => false}}
3> example_users:verify("meppu").
{ok,#{<<"age">> => 16,<<"id">> => <<"users:meppu">>,
      <<"verified">> => true}}
4> example_users:add("tuhana", 18).
{ok,#{<<"age">> => 18,<<"id">> => <<"users:tuhana">>,
      <<"verified">> => false}}
5> example_users:get_all().
{ok,[#{<<"age">> => 16,<<"id">> => <<"users:meppu">>,
       <<"verified">> => true},
     #{<<"age">> => 18,<<"id">> => <<"users:tuhana">>,
       <<"verified">> => false}]}
6> example_users:remove("meppu").
{ok,#{<<"age">> => 16,<<"id">> => <<"users:meppu">>,
      <<"verified">> => true}}
7> example_users:get_all().
{ok,[#{<<"age">> => 18,<<"id">> => <<"users:tuhana">>,
       <<"verified">> => false}]}
```
